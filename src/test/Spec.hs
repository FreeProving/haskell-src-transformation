module Spec where

import           Test.Hspec              ( hspec )

import           HST.ApplicationTests    ( testApplication )
import           HST.CoreAlgorithmTests  ( testCoreAlgorithm )
import           HST.Effect.CancelTests  ( testCancelEffect )
import           HST.Effect.FreshTests   ( testFreshEffect )
import           HST.Effect.ReportTests  ( testReportEffect )
import           HST.Util.FreeVarsTests  ( testFreeVars )
import           HST.Util.MessagesTests ( testMessages )
import           HST.Util.SelectorsTests ( testSelectors )
import           HST.Util.SubstTests     ( testSubst )


main :: IO ()
main = hspec $ do
  testApplication
  testCancelEffect
  testCoreAlgorithm
  testFreshEffect
  testFreeVars
  testMessages
  testReportEffect
  testSelectors
  testSubst
