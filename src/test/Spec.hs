module Spec where

import           Test.Hspec

import           AlgoTests
import           BasicTests

main :: IO ()
main = do
  hspec testAlgo
  hspec basicTests
