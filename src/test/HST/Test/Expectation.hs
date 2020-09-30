-- | This module contains utility functions for setting expectations in tests.
module HST.Test.Expectation where

import           Polysemy                  ( Members, Sem )
import           Test.Hspec                ( shouldBe )

import           HST.Effect.SetExpectation ( SetExpectation, setExpectation )
import           HST.Effect.WithFrontend
  ( WithFrontend, prettyPrintExp, prettyPrintModule )
import qualified HST.Frontend.Syntax       as S

-- | Pretty prints both given modules and tests whether the resulting strings
--   are equal modulo whitespace.
prettyModuleShouldBe :: Members '[SetExpectation, WithFrontend f] r
                     => S.Module f
                     -> S.Module f
                     -> Sem r ()
prettyModuleShouldBe m1 m2 = do
  p1 <- prettyPrintModule m1
  p2 <- prettyPrintModule m2
  setExpectation (p1 `shouldBe` p2)

-- | Pretty prints both given expressions and tests whether the resulting
--   strings are equal modulo whitespace.
prettyExpShouldBe :: Members '[SetExpectation, WithFrontend f] r
                  => S.Exp f
                  -> S.Exp f
                  -> Sem r ()
prettyExpShouldBe e1 e2 = do
  p1 <- prettyPrintExp e1
  p2 <- prettyPrintExp e2
  setExpectation (p1 `shouldBe` p2)
