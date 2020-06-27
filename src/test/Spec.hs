module Spec where

import           Test.Hspec                     ( hspec )

import           HST.CoreAlgorithmTests         ( testAlgo )

main :: IO ()
main = hspec testAlgo
