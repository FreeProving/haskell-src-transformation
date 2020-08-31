{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module defines an effect for computations that can report 'Message's
--   such as errors and warnings at runtime.
--
--   There are handlers for such computations that either collect the reported
--   messages or print them to a file 'Handle'. Reported messages can be either
--   fatal or non-fatal. The usual interpretation of a fatal message is for
--   the computation to be 'cancel'ed.
module HST.Effect.Report
  ( -- * Messages
    Severity(..)
  , Message(..)
  , message
  , showPrettyMessage
    -- * Effect
  , Report
    -- * Actions
  , report
  , reportFatal
    -- * Interpretations
  , runReport
  , evalReport
  , reportToOutputOrCancel
  , reportToHandleOrCancel
  , filterReportedMessages
    -- * Interpretations for Other Effects
  , cancelToReport
  , errorToReport
  , exceptionToReport
  , failToReport
  ) where

import           Control.Exception    ( Exception )
import           Control.Monad        ( (>=>), when )
import           Data.Char            ( isSpace )
import           Polysemy
  ( Member, Members, Sem, intercept, interpret, makeSem, raise, raiseUnder2 )
import           Polysemy.Embed       ( Embed, embed )
import           Polysemy.Error       ( Error, fromExceptionSem, runError )
import           Polysemy.Fail        ( Fail, runFail )
import           Polysemy.Final       ( Final )
import           Polysemy.Output
  ( Output, ignoreOutput, output, runOutputList )
import           System.IO            ( Handle, hPutStrLn )

import           HST.Effect.Cancel    ( Cancel, cancel, runCancel )
import           HST.Effect.InputFile ( InputFile, getInputFile )
import qualified HST.Frontend.Syntax  as S

-------------------------------------------------------------------------------
-- Messages                                                                  --
-------------------------------------------------------------------------------
-- | The severity of a 'Message'.
data Severity = Internal | Error | Warning | Info | Debug
 deriving ( Show, Eq )

-- | A message that can be 'report'ed.
data Message = Message { msgSeverity :: Severity
                       , msgSrcSpan  :: Maybe S.MsgSrcSpan
                       , msgText     :: String
                       }
 deriving ( Show, Eq )

-- | Like using the 'Message' constructor directly, but takes a 'S.SrcSpan'
--   instead of a @Maybe@ 'S.MsgSrcSpan' and does the conversion.
message :: Severity -> S.SrcSpan a -> String -> Message
message severity srcSpan = Message severity (S.toMsgSrcSpan srcSpan)

-- TODO Add @Pretty@ instance for messages.
showPrettyMessage :: Member InputFile r => Message -> Sem r String
showPrettyMessage (Message severity srcSpan msg) = do
  excerpt <- displayCodeExcerpt srcSpan
  return
    $ show severity
    ++ ": "
    ++ msg
    ++ if null excerpt then "" else '\n' : excerpt

-- TODO Should the following function be in its own module? It can't be put
-- back into 'HST.Frontend.Transformer.Messages' because of cyclic imports.

-- | Displays an excerpt of the input code specified by the given source span.
--
--   The excerpt consists of an introductory line with the file path and the
--   source span numbers, all lines of the input program that are at least
--   partially contained in the given source span, including their line
--   numbers, and marks showing the start and end of the spanned code.
displayCodeExcerpt :: Member InputFile r => Maybe S.MsgSrcSpan -> Sem r String
displayCodeExcerpt Nothing = return ""
displayCodeExcerpt (Just src) = do
  maybeContent <- getInputFile (S.msgSrcSpanFilePath src)
  return $ case maybeContent of
    Nothing      -> ""
    Just content ->
      let ls = getLines (S.msgSrcSpanStartLine src - 1) (S.msgSrcSpanEndLine src)
            (lines content)
      in if not (isValidSrcSpan ls)
           then "The source span "
             ++ srcString
             ++ " of `"
             ++ S.msgSrcSpanFilePath src
             ++ "` cannot be fully displayed!"
           else S.msgSrcSpanFilePath src
             ++ ':' : srcString ++ ":\n" ++ unlines (prettyLines ls)
 where
  -- | Returns a sublist of the given list specified by the given indices.
  --
  --   The first of the zero-based indices is inclusive, the second is
  --   exclusive. Invalid indices do not cause runtime errors.
  --   Example: getLines 1 3 [1, 2, 3, 4, 5] = [2, 3]
  getLines :: Int -> Int -> [a] -> [a]
  getLines i1 i2 = take (i2 - i1) . drop i1

  -- | Tests if a source span is valid by checking if the locations specified
  --   in the source span do exist in the given lines.
  isValidSrcSpan :: [String] -> Bool
  isValidSrcSpan [] = False
  isValidSrcSpan ls
    = (length ls == S.msgSrcSpanEndLine src - S.msgSrcSpanStartLine src + 1)
    && (length (head ls) >= S.msgSrcSpanStartColumn src)
    && (length (last ls) >= S.msgSrcSpanEndColumn src - 1)

  -- | Builds a string displaying the line and column of the source span start
  --   and end.
  srcString :: String
  srcString = show (S.msgSrcSpanStartLine src)
    ++ ':'
    : show (S.msgSrcSpanStartColumn src)
    ++ '-' : show (S.msgSrcSpanEndLine src) ++ ':' : show (S.msgSrcSpanEndColumn src)

  -- | Adds line numbers to each given line and adds marks showing the start
  --   and end of the spanned code.
  prettyLines :: [String] -> [String]
  prettyLines ls
    = let (numbers, maxLineNumLength) = alignedLineNumbers
            (S.msgSrcSpanStartLine src) (S.msgSrcSpanEndLine src)
          lsWithNum                   = zipWith
            (\lNum code -> lNum ++ " | " ++ code) numbers ls
      in case ls of
           -- Excerpts consisting of no lines should be exfiltrated by
           -- 'isValidSrcSpan'.
           []        -> []
           -- For single-line excerpts, an additional line below the excerpt
           -- marking the entire spanned code is added.
           [_]       ->
             let lastLine = replicate
                   (maxLineNumLength + 2 + S.msgSrcSpanStartColumn src) ' '
                   ++ replicate
                   (S.msgSrcSpanEndColumn src - S.msgSrcSpanStartColumn src) '^'
             in lsWithNum ++ [lastLine]
           -- For multi-line excerpts, there are two lines added:
           -- A line above the excerpt marking the start of the spanned code.
           -- A line below the excerpt marking the end of the spanned code.
           -- For excerpts containing more than five lines, only the first two
           -- and last two lines are shown and an additional line marking the
           -- abbreviation is added between them.
           _ : _ : _ ->
             let lsLength      = length ls
                 lsShort       = if lsLength <= 5
                   then ls
                   else take 2 ls ++ drop (lsLength - 2) ls
                 maxLineLength = maximum
                   (S.msgSrcSpanEndColumn src - 1 : map length (init lsShort))
                 minPadding    = minimum
                   (S.msgSrcSpanStartColumn src - 1
                    : map (length . takeWhile isSpace)
                    (filter (not . all isSpace) (tail lsShort)))
                 firstLine     = replicate
                   (maxLineNumLength + 2 + S.msgSrcSpanStartColumn src) ' '
                   ++ replicate (maxLineLength - S.msgSrcSpanStartColumn src + 1)
                   'v'
                 lastLine      = replicate (maxLineNumLength + 3 + minPadding)
                   ' '
                   ++ replicate (S.msgSrcSpanEndColumn src - minPadding - 1) '^'
             in if lsLength <= 5
                  then firstLine : lsWithNum ++ [lastLine]
                  else let abbrLine = replicate (maxLineNumLength - 1) ' '
                             ++ ":"
                       in firstLine
                          : take 2 lsWithNum
                          ++ abbrLine
                          : drop (lsLength - 2) lsWithNum ++ [lastLine]

  -- | Produces a list of right-aligned line numbers going from the first to
  --   the second given line number (both are inclusive). Also returns the
  --   maximum length of these numbers (the length of the second given number).
  alignedLineNumbers :: Int -> Int -> ([String], Int)
  alignedLineNumbers l1 l2
    = let maxLineNumLength = length (show l2)
      in (map (padLeft maxLineNumLength . show) [l1 .. l2], maxLineNumLength)

  -- | Adds spaces to the left side of the given string so that the given
  --   maximum length is reached.
  padLeft :: Int -> String -> String
  padLeft maxLength s = replicate (maxLength - length s) ' ' ++ s

-------------------------------------------------------------------------------
-- Effect and Actions                                                        --
-------------------------------------------------------------------------------
-- | An effect capable of reporting 'Message's (e.g., errors and warnings).
--
--   It is distinguished between fatal and non-fatal messages. A fatal message
--   is usually an error that cannot be recovered from.
data Report m a where
  Report :: Message -> Report m ()
  ReportFatal :: Message -> Report m a

makeSem ''Report

-------------------------------------------------------------------------------
-- Interpretations                                                           --
-------------------------------------------------------------------------------
-- | Handles the 'Report' effect by collecting all reported messages in a list.
--
--   The return value of the handled computation is wrapped in @Maybe@.
--   If a fatal message is reported, @Nothing@ is returned and only the
--   messages up to the fatal message are collected.
runReport :: Sem (Report ': r) a -> Sem r ([Message], Maybe a)
runReport = runOutputList . runCancel . reportToOutputOrCancel . raiseUnder2

-- | Handles the 'Report' effect by discarding all reported messages.
--
--   The return value of the handled computation is wrapped in @Maybe@.
--   If a fatal message is reported, @Nothing@ is returned.
evalReport :: Sem (Report ': r) a -> Sem r (Maybe a)
evalReport = ignoreOutput . runCancel . reportToOutputOrCancel . raiseUnder2

-- | Handles the 'Report' effect by 'output'ing all reported messages.
--
--   If a fatal message is reported, the computation is 'cancel'ed
--   prematurely.
reportToOutputOrCancel
  :: Members '[Output Message, Cancel] r => Sem (Report ': r) a -> Sem r a
reportToOutputOrCancel = interpret \case
  Report msg      -> output msg
  ReportFatal msg -> output msg >> cancel

-- | Handles the 'Report' effect by printing reported messages to the given
--   file 'Handle'.
--
--   If a fatal message is reported, the computation is 'cancel'ed
--   prematurely.
reportToHandleOrCancel :: Members '[Embed IO, Cancel, InputFile] r
                       => Handle
                       -> Sem (Report ': r) a
                       -> Sem r a
reportToHandleOrCancel h = interpret \case
  Report msg      -> hPutMessage msg
  ReportFatal msg -> hPutMessage msg >> cancel
 where
  -- | Prints the given message to the file handle given to the effect handler.
  hPutMessage :: Members '[Embed IO, InputFile] r => Message -> Sem r ()
  hPutMessage msg = do
    msg' <- showPrettyMessage msg
    embed (hPutStrLn h msg')

-- | Intercepts all non-fatal messages reported by the given computation and
--   forwards them only if they satisfy the given predicate.
filterReportedMessages
  :: Member Report r => (Message -> Bool) -> Sem r a -> Sem r a
filterReportedMessages p = intercept \case
  Report msg      -> when (p msg) (report msg)
  ReportFatal msg -> reportFatal msg

-------------------------------------------------------------------------------
-- Interpretations for Other Effects                                         --
-------------------------------------------------------------------------------
-- | Handles the 'Cancel' effect by reporting the given fatal error message
--   when the computation was canceled.
cancelToReport
  :: Member Report r => Message -> Sem (Cancel ': r) a -> Sem r a
cancelToReport cancelMessage = runCancel
  >=> maybe (reportFatal cancelMessage) return

-- | Handles the 'Error' effect by reporting thrown errors as the message
--   returned by the given function for the error.
errorToReport
  :: Member Report r => (e -> Message) -> Sem (Error e ': r) a -> Sem r a
errorToReport errorToMessage = runError
  >=> either (reportFatal . errorToMessage) return

-- | Handles exceptions thrown by IO actions embedded into the given computation
--   by reporting the message returned by the given function for the thrown
--   exception.
exceptionToReport :: (Exception e, Members '[Final IO, Report] r)
                  => (e -> Message)
                  -> Sem r a
                  -> Sem r a
exceptionToReport exceptionToMessage
  = errorToReport exceptionToMessage . fromExceptionSem . raise

-- | Handles the 'Fail' effect by reporting a internal fatal error if the
--   given computation fails.
--
--   This handler can be used to report pattern matching failures in @do@
--   blocks as shown in the example below since there is a @MonadFail@ instance
--   for @Member Fail r => Sem r a@.
--
--   > foo :: Member Report r => Sem r a
--   > foo = failToReport $ do
--   >   (x, y) <- bar
--   >   …
failToReport :: Member Report r => Sem (Fail ': r) a -> Sem r a
failToReport = runFail
  >=> either (reportFatal . message Internal S.NoSrcSpan) return
