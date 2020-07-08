module Spec where

import           Test.Hspec                     ( hspec )

import           HST.CoreAlgorithmTests         ( testCoreAlgorithm )
import           HST.Effect.CancelTests         ( testCancelEffect )
import           HST.Effect.ReportTests         ( testReportEffect )

main :: IO ()
main = hspec $ do
  testCoreAlgorithm
  testCancelEffect
  testReportEffect
