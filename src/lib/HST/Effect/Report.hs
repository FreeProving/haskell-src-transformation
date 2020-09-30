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
  ( -- * Effect
    Report(..)
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
import           HST.Effect.InputFile ( InputFile )
import           HST.Util.Messages
  ( Message, Severity(Internal), messageWithoutSrcSpan
  , showPrettyMessageWithExcerpt )

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
    msg' <- showPrettyMessageWithExcerpt msg
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
cancelToReport :: Member Report r => Message -> Sem (Cancel ': r) a -> Sem r a
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
  >=> either (reportFatal . messageWithoutSrcSpan Internal) return
