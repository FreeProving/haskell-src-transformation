module Spec where

import           Test.Hspec                     ( hspec )

import           HST.CoreAlgorithmTests         ( testAlgo )
import           HST.ApplicationTests           ( applicationTests )

main :: IO ()
main = hspec $ do
  testAlgo
  applicationTests
