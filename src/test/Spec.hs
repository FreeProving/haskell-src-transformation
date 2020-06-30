module Spec where

import           Test.Hspec                     ( hspec )

import           HST.CoreAlgorithmTests         ( testCoreAlgorithm )
import           HST.Effect.CancelTests         ( testCancelEffect )

main :: IO ()
main = hspec $ do
  testCoreAlgorithm
  testCancelEffect
