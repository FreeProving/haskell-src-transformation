module Spec where

import           Test.Hspec                     ( hspec )

import           HST.CoreAlgorithmTests         ( testAlgo )
import           BasicTests                     ( basicTests )

main :: IO ()
main = do
  hspec testAlgo
  hspec basicTests
