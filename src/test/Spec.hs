module Spec where

import           Test.Hspec                     ( hspec )

import           HST.AlgoTests                      ( testAlgo )

main :: IO ()
main = hspec testAlgo
