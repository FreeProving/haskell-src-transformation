-- | This module contains utility functions for setting expectations in tests.
module HST.Test.Expectation where

import           Polysemy                  ( Members, Sem )
import           Test.Hspec                ( shouldBe )

import           HST.Effect.SetExpectation ( SetExpectation, setExpectation )
import           HST.Effect.WithFrontend
  ( WithFrontend, prettyPrintExp, prettyPrintModule
  , unTransformExp, unTransformModule )
import qualified HST.Frontend.Syntax       as S

-- | Pretty prints both given modules and tests whether the resulting strings
--   are equal modulo whitespace.
prettyModuleShouldBe :: Members '[SetExpectation, WithFrontend f] r
                     => S.Module f
                     -> S.Module f
                     -> Sem r ()
prettyModuleShouldBe m1 m2 = do
  p1 <- unTransformModule m1 >>= prettyPrintModule
  p2 <- unTransformModule m2 >>= prettyPrintModule
  setExpectation (p1 `shouldBe` p2)

-- | Pretty prints both given expressions and tests whether the resulting
--   strings are equal modulo whitespace.
prettyExpShouldBe :: Members '[SetExpectation, WithFrontend f] r
                         => S.Exp f
                         -> S.Exp f
                         -> Sem r ()
prettyExpShouldBe e1 e2 = do
  p1 <- unTransformExp e1 >>= prettyPrintExp
  p2 <- unTransformExp e2 >>= prettyPrintExp
  setExpectation (p1 `shouldBe` p2)
