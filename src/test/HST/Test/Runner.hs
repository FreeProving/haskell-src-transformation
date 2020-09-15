{-# LANGUAGE RankNTypes #-}

-- | This module contains utility functions to run computations for testing
--   purposes.
module HST.Test.Runner where

import           Polysemy                  ( Sem, runM )
import           Polysemy.Embed            ( Embed )

import           HST.Effect.Cancel         ( Cancel )
import           HST.Effect.GetOpt         ( GetOpt, runWithArgs )
import           HST.Effect.Report
  ( Report, cancelToReport )
import           HST.Effect.SetExpectation
  ( SetExpectation, reportToSetExpectation, setExpectationToIO )
import           HST.Effect.WithFrontend   ( WithFrontend, runWithAllFrontends )
import qualified HST.Frontend.Syntax       as S
import           HST.Util.Messages (Severity(Info), message)

-- | Runs the given computation with an empty environment and no additional
--   command line arguments.
runTest
  :: (forall f.
      (S.EqAST f, S.ShowAST f)
      => Sem '[WithFrontend f, GetOpt, Cancel, Report, SetExpectation, Embed IO]
      ())
  -> IO ()
runTest comp = runM
  $ setExpectationToIO
  $ reportToSetExpectation
  $ cancelToReport (message Info S.NoSrcSpan "The computation was canceled.")
  $ runWithArgs []
  $ runWithAllFrontends comp
