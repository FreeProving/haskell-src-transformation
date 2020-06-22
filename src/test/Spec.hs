module Spec where

import           Test.Hspec                     ( hspec )

import           AlgoTests                      ( testAlgo )

main :: IO ()
main = hspec testAlgo
