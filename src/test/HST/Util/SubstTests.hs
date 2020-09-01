-- | This module contains tests for "HST.Util.Subst".
module HST.Util.SubstTests where

import           Polysemy                  ( Members, Sem )
import           Test.Hspec                ( Spec, context, describe, it )

import           HST.Effect.Cancel         ( Cancel )
import           HST.Effect.Report         ( Report )
import           HST.Effect.SetExpectation ( SetExpectation )
import           HST.Effect.WithFrontend   ( WithFrontend )
import qualified HST.Frontend.Syntax       as S
import           HST.Test.Expectation      ( prettyExpressionShouldBe )
import           HST.Test.Parser           ( parseTestExpression )
import           HST.Test.Runner           ( runTest )
import           HST.Util.Subst
  ( Subst, applySubst, composeSubst, singleSubst, substFromList )

-------------------------------------------------------------------------------
-- Test Values                                                               --
-------------------------------------------------------------------------------
-- | Name of a test variable @x@.
x :: S.Name a
x = S.Ident S.NoSrcSpan "x"

-- | Name of a test variable @y@.
y :: S.Name a
y = S.Ident S.NoSrcSpan "y"

-- | Name of a test variable @z@.
z :: S.Name a
z = S.Ident S.NoSrcSpan "z"

-------------------------------------------------------------------------------
-- Expectation Setters                                                       --
-------------------------------------------------------------------------------
-- | Parses the given expressions, applies the given substitution to the first
--   expression and sets the expectation that the given output expression is
--   produced.
shouldSubstituteTo
  :: (S.EqAST f, Members '[Cancel, Report, SetExpectation, WithFrontend f] r)
  => Subst f
  -> [String]
  -> [String]
  -> Sem r ()
shouldSubstituteTo subst input expectedOutput = do
  inputExp <- parseTestExpression input
  let outputExp = applySubst subst inputExp
  expectedOutputExp <- parseTestExpression expectedOutput
  outputExp `prettyExpressionShouldBe` expectedOutputExp

-------------------------------------------------------------------------------
-- Tests                                                                     --
-------------------------------------------------------------------------------
-- | Test group for "HST.Util.FreeVars".
testSubst :: Spec
testSubst = describe "HST.Util.Subst" $ do
  context "composeSubst" $ do
    it "applies the second substitution first"
      $ runTest
      $
      -- σ₁   = { x ↦ z }
      -- σ₂   = { x ↦ y }
      -- σ    = σ₁ . σ₂ = { x ↦ y }
      -- e    = x y z
      -- σ(e) = y y z
      let s1       = singleSubst (S.unQual x) (S.var z)
          s2       = singleSubst (S.unQual x) (S.var y)
          subst    = composeSubst s1 s2
          e        = ["x y z"]
          expected = ["y y z"]
      in shouldSubstituteTo subst e expected
    it "applies the second substitution to the first"
      $ runTest
      $
      -- σ₁   = { y ↦ z }
      -- σ₂   = { x ↦ y }
      -- σ    = σ₁ . σ₂ = { x ↦ z, y ↦ z }
      -- e    = x y z
      -- σ(e) = z z z
      let s1       = singleSubst (S.unQual y) (S.var z)
          s2       = singleSubst (S.unQual x) (S.var y)
          subst    = composeSubst s1 s2
          e        = ["x y z"]
          expected = ["z z z"]
      in shouldSubstituteTo subst e expected
  context "applySubst" $ do
    it "substitutes variables"
      $ runTest
      $
      -- σ    = { x ↦ y }
      -- e    = f x
      -- σ(e) = f y
      let subst    = singleSubst (S.unQual x) (S.var y)
          e        = ["f x"]
          expected = ["f y"]
      in shouldSubstituteTo subst e expected
    it "does not substitute bound variables"
      $ runTest
      $
      -- σ    = { x ↦ z, y ↦ z }
      -- e    = (\x -> f x y, x, y)
      -- σ(e) = (\x -> f x z, z, z)
      let subst
            = substFromList [(S.unQual x, S.var z), (S.unQual y, S.var z)]
          e        = ["(\\x -> f x y, x, y)"]
          expected = ["(\\x -> f x z, z, z)"]
      in shouldSubstituteTo subst e expected
    it "renames bound variables to avoid capture"
      $ runTest
      $
      -- σ    = { y ↦ x }
      -- e    = \x   -> (x, y)
      -- σ(e) = \x_0 -> (x_0, x)
      let subst    = singleSubst (S.unQual y) (S.var x)
          e        = ["\\x   -> (x, y)"]
          expected = ["\\x_0 -> (x_0, x)"]
      in shouldSubstituteTo subst e expected
    it "renames bound variables only if necessary"
      $ runTest
      $
      -- σ    = { y ↦ z }
      -- e    = \x -> (x, y)
      -- σ(e) = \x -> (x, z)
      let subst    = singleSubst (S.unQual y) (S.var z)
          e        = ["\\x -> (x, y)"]
          expected = ["\\x -> (x, z)"]
      in shouldSubstituteTo subst e expected
    it "does not rename bound variables that capture unused free variables"
      $ runTest
      $
      -- σ    = { x ↦ y }
      -- e    = \y -> y
      -- σ(e) = \y -> y
      let subst    = singleSubst (S.unQual x) (S.var y)
          e        = ["\\y -> y"]
          expected = ["\\y -> y"]
      in shouldSubstituteTo subst e expected
    it "avoids capture by renaming variables"
      $ runTest
      $
      -- σ    = { y ↦ x }
      -- e    = \x   -> (x, x_0, y)
      -- σ(e) = \x_1 -> (x_1, x_0, x)
      let subst    = singleSubst (S.unQual y) (S.var x)
          e        = ["\\x   -> (x, x_0, y)"]
          expected = ["\\x_1 -> (x_1, x_0, x)"]
      in shouldSubstituteTo subst e expected
    it "avoids capture of renamed variables"
      $ runTest
      $
      -- σ    = { y ↦ x }
      -- e    = \x   x_0 -> (x, x_0, y)
      -- σ(e) = \x_0 x_1 -> (x_0, x_1, x)
      let subst    = singleSubst (S.unQual y) (S.var x)
          e        = ["\\x   x_0 -> (x, x_0, y)"]
          expected = ["\\x_0 x_1 -> (x_0, x_1, x)"]
      in shouldSubstituteTo subst e expected