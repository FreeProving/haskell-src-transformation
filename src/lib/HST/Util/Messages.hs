-- | This module contains data types for messages and message building
--   functions that can be used to present various kinds of errors and other
--   information.
module HST.Util.Messages
  ( Severity(..)
  , Message(..)
  , displayCodeExcerpt
  , message
  , messageWithoutSrcSpan
  , showPrettyMessage
  , showPrettyMessageWithExcerpt
  ) where

import           Data.Char            ( isSpace )
import           Polysemy             ( Member, Sem )

import           HST.Effect.InputFile ( InputFile, getInputFile )
import qualified HST.Frontend.Syntax  as S

-------------------------------------------------------------------------------
-- Messages                                                                  --
-------------------------------------------------------------------------------
-- | The severity of a 'Message'.
data Severity = Internal | Error | Warning | Info | Debug
 deriving ( Show, Eq )

-- | A message that can be 'HST.Effect.Report.report'ed.
data Message = Message { msgSeverity :: Severity
                       , msgSrcSpan  :: Maybe S.MsgSrcSpan
                       , msgText     :: String
                       }
 deriving ( Show, Eq )

-- | Like using the 'Message' constructor directly, but takes a 'S.SrcSpan'
--   instead of a @Maybe@ 'S.MsgSrcSpan' and does the conversion.
message :: Severity -> S.SrcSpan a -> String -> Message
message severity srcSpan = Message severity (S.toMsgSrcSpan srcSpan)

messageWithoutSrcSpan :: Severity -> String -> Message
messageWithoutSrcSpan severity = Message severity Nothing

-- TODO Add @Pretty@ instance for messages.
-- | Pretty-prints a message without a code excerpt.
--
--   Only used for testing purposes where the 'HST.Effect.InputFile' effect is
--   not always handled.
showPrettyMessage :: Message -> String
showPrettyMessage (Message severity Nothing msg)
  = show severity ++ ": " ++ msg
showPrettyMessage (Message severity (Just src) msg)
  = show severity ++ ": " ++ msg ++ "\n  In " ++ prettyMsgSrcSpan src ++ "."

-- | Pretty-prints a message with a code excerpt.
showPrettyMessageWithExcerpt :: Member InputFile r => Message -> Sem r String
showPrettyMessageWithExcerpt (Message severity srcSpan msg) = do
  excerpt <- displayCodeExcerpt srcSpan
  return
    $ show severity
    ++ ": "
    ++ msg
    ++ if null excerpt then "" else '\n' : excerpt

-- | Builds a string displaying the file path and the line and column of the
--   start and end of the given message source span.
prettyMsgSrcSpan :: S.MsgSrcSpan -> String
prettyMsgSrcSpan src
  = S.msgSrcSpanFilePath src ++ ':' : prettyMsgSrcSpanNumbers src

-- | Builds a string displaying the line and column of the given message source
--   span start and end.
prettyMsgSrcSpanNumbers :: S.MsgSrcSpan -> String
prettyMsgSrcSpanNumbers src = show (S.msgSrcSpanStartLine src)
  ++ ':'
  : show (S.msgSrcSpanStartColumn src)
  ++ '-'
  : show (S.msgSrcSpanEndLine src) ++ ':' : show (S.msgSrcSpanEndColumn src)

-- | Displays an excerpt of the input code specified by the given source span.
--
--   The excerpt consists of an introductory line with the file path and the
--   source span numbers, up to five of the lines of the input program that are
--   at least partially contained in the given source span, including their
--   line numbers, and marks showing the start and end of the spanned code.
displayCodeExcerpt :: Member InputFile r => Maybe S.MsgSrcSpan -> Sem r String
displayCodeExcerpt Nothing = return ""
displayCodeExcerpt (Just src) = do
  contents <- getInputFile (S.msgSrcSpanFilePath src)
  return
    $ let ls = getLines (S.msgSrcSpanStartLine src - 1)
            (S.msgSrcSpanEndLine src) (lines contents)
      in if not (isValidSrcSpan ls)
           then "The source span "
             ++ prettyMsgSrcSpanNumbers src
             ++ " of `"
             ++ S.msgSrcSpanFilePath src
             ++ "` cannot be fully displayed!"
           else prettyMsgSrcSpan src ++ ":\n" ++ unlines (prettyLines ls)
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
  isValidSrcSpan ls = length ls
    == S.msgSrcSpanEndLine src - S.msgSrcSpanStartLine src + 1
    && length (head ls) >= S.msgSrcSpanStartColumn src
    && length (last ls) >= S.msgSrcSpanEndColumn src - 1
    && (S.msgSrcSpanEndLine src > S.msgSrcSpanStartLine src
        || S.msgSrcSpanEndColumn src > S.msgSrcSpanStartColumn src)

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
                   ++ replicate
                   (maxLineLength - S.msgSrcSpanStartColumn src + 1) 'v'
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
