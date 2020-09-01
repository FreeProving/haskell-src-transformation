module Spec where

import           Test.Hspec             ( hspec )

import           HST.ApplicationTests   ( testApplication )
import           HST.CoreAlgorithmTests ( testCoreAlgorithm )
import           HST.Effect.CancelTests ( testCancelEffect )
import           HST.Effect.InputFileTests ( testInputFileEffect )
import           HST.Effect.ReportTests ( testReportEffect )

main :: IO ()
main = hspec $ do
  testApplication
  testCoreAlgorithm
  testCancelEffect
  testInputFileEffect
  testReportEffect
