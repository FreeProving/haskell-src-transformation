-- | This module contains tests for "HST.Util.Subst".
module HST.Util.SubstTests where

import           Polysemy                  ( Members, Sem )
import           Test.Hspec                ( Spec, context, describe, it )

import           HST.Effect.Cancel         ( Cancel )
import           HST.Effect.Report         ( Report )
import           HST.Effect.SetExpectation ( SetExpectation )
import           HST.Effect.WithFrontend   ( WithFrontend )
import qualified HST.Frontend.Syntax       as S
import           HST.Test.Expectation      ( prettyExpressionShouldBe, shouldTransformTo )
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
      -- σ₁ = { x ↦ z }
      -- σ₂ = { x ↦ y }
      -- σ  = σ₁ . σ₂ = { x ↦ y }
      let s1       = singleSubst (S.unQual x) (S.var z)
          s2       = singleSubst (S.unQual x) (S.var y)
          subst    = composeSubst s1 s2
          e        = ["x y z"]
          expected = ["y y z"]
      in shouldSubstituteTo subst e expected
    it "applies the second substitution to the first"
      $ runTest
      $
      -- σ₁ = { y ↦ z }
      -- σ₂ = { x ↦ y }
      -- σ  = σ₁ . σ₂ = { x ↦ z, y ↦ z }
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
      -- σ = { x ↦ y }
      let subst    = singleSubst (S.unQual x) (S.var y)
          e        = ["f x"]
          expected = ["f y"]
      in shouldSubstituteTo subst e expected
    it "does not substitute bound variables"
      $ runTest
      $
      -- σ = { x ↦ z, y ↦ z }
      let subst
            = substFromList [(S.unQual x, S.var z), (S.unQual y, S.var z)]
          e        = ["(\\x -> f x y, x, y)"]
          expected = ["(\\x -> f x z, z, z)"]
      in shouldSubstituteTo subst e expected
    it "renames bound variables to avoid capture"
      $ runTest
      $
      -- σ = { y ↦ x }
      let subst    = singleSubst (S.unQual y) (S.var x)
          e        = ["\\x   -> (x, y)"]
          expected = ["\\x_0 -> (x_0, x)"]
      in shouldSubstituteTo subst e expected
    it "renames bound variables only if necessary"
      $ runTest
      $
      -- σ = { y ↦ z }
      let subst    = singleSubst (S.unQual y) (S.var z)
          e        = ["\\x -> (x, y)"]
          expected = ["\\x -> (x, z)"]
      in shouldSubstituteTo subst e expected
    it "does not rename bound variables that capture unused free variables"
      $ runTest
      $
      -- σ = { x ↦ y }
      let subst    = singleSubst (S.unQual x) (S.var y)
          e        = ["\\y -> y"]
          expected = ["\\y -> y"]
      in shouldSubstituteTo subst e expected
    it "avoids capture by renaming variables"
      $ runTest
      $
      -- σ = { y ↦ x }
      let subst    = singleSubst (S.unQual y) (S.var x)
          e        = ["\\x   -> (x, x_0, y)"]
          expected = ["\\x_1 -> (x_1, x_0, x)"]
      in shouldSubstituteTo subst e expected
    it "avoids capture of renamed variables"
      $ runTest
      $
      -- σ = { y ↦ x }
      let subst    = singleSubst (S.unQual y) (S.var x)
          e        = ["\\x   x_0 -> (x, x_0, y)"]
          expected = ["\\x_0 x_1 -> (x_0, x_1, x)"]
      in shouldSubstituteTo subst e expected
    it "does not substitute bound variables in let expressions"
      $ runTest
      $
      -- σ = { x ↦ z, y ↦ z }
      let subst    = substFromList [(S.unQual x, S.var z), (S.unQual y, S.var z)]
          e        = ["let x = z in x y"]
          expected = ["let x = z in x z"]
      in shouldSubstituteTo subst e expected
    it "renames variables that are no longer bound"
      $ runTest
      $
      -- σ = { y ↦ z }
      let subst    = singleSubst (S.unQual y) (S.var z)
          e        = [ "let x = let y = z in y"
                     , "in x y"]
          expected = [ "let x = let y = z in y"
                     , "in x z"]
      in shouldSubstituteTo subst e expected
    it "does not substitute bound variables in nested let expressions"
      $ runTest
      $
      -- σ = { x ↦ z, y ↦ z }
      let subst    = substFromList [(S.unQual x, S.var z), (S.unQual y, S.var z)]
          e        = [ "let x = y"
                     , "in let y = z in x y"]
          expected = [ "let x = z"
                     , "in let y = z in x y"]
      in shouldSubstituteTo subst e expected
    it "does not use more variable names than needed"
      $ runTest
      $
      -- σ = { y ↦ x }
      let subst    = singleSubst (S.unQual y) (S.var x)
          e        = [ "let x = y"
                     , "in let x = y in x"]
          expected = [ "let x_0 = x"
                     , "in let x_0 = x in x_0"]
      in shouldSubstituteTo subst e expected
    it "avoids capture by renaming variables in let expressions"
      $ runTest
      $
      -- σ = { y ↦ x, z ↦ x }
      let subst    = substFromList [(S.unQual y, S.var x), (S.unQual z, S.var x)]
          e        = [ "let x   = y"
                     , "    x_0 = z"
                     , "in  x_1"]
          expected = [ "let x_0 = x"
                     , "    x_2 = x"
                     , "in  x_1"]
      in shouldSubstituteTo subst e expected
    it "only substitutes variables not bound by a case alternative pattern"
      $ runTest
      $
      -- σ = { x ↦ z, y ↦ z }
      let subst    = substFromList [(S.unQual x, S.var z), (S.unQual y, S.var z)]
          e        = [ "case x y of"
                     , "  (x, x_0) -> (x, y)"]
          expected = [ "case z z of"
                     , "  (x, x_0) -> (x, z)"]
      in shouldSubstituteTo subst e expected
    it "uses different name spaces for each case alternative"
      $ runTest
      $
      -- σ = { x ↦ y }
      let subst    = singleSubst (S.unQual x) (S.var y)
          e        = [ "case x of"
                     , "  [y]   -> x"
                     , "  y : z -> y : x"]
          expected = [ "case y of"
                     , "  [y_0]   -> y"
                     , "  y_0 : z -> y_0 : y"]
      in shouldSubstituteTo subst e expected
    it ("only renames bound variables in case alternatives where the free "
          ++ "variables they capture are used")
      $ runTest
      $
      -- σ = { x ↦ y }
      let subst    = singleSubst (S.unQual x) (S.var y)
          e        = [ "case x of"
                     , "  y -> x y"
                     , "  y -> let x = z in x y"]
          expected = [ "case y of"
                     , "  y_0 -> y y_0"
                     , "  y   -> let x = z in x y"]
      in shouldSubstituteTo subst e expected
    it "avoids capture by renaming variables in case alternative patterns"
      $ runTest
      $
      -- σ = { y ↦ x, z ↦ x }
      let subst    = substFromList [(S.unQual y, S.var x), (S.unQual z, S.var x)]
          e        = [ "case [y, z] of"
                     , "  x : x_2 : x_0 -> (x, y, z)"]
          expected = [ "case [x, x] of"
                     , "  x_0 : x_2 : x_1 -> (x_0, x, x)"]
      in shouldSubstituteTo subst e expected
    it "does not rename variables bound in where clauses"
      $ runTest
      $
      -- σ = { z ↦ x }
      let subst    = singleSubst (S.unQual z) (S.var x)
          e        = [ "case x of"
                     , "  y -> z"
                     , "   where z = x"]
          expected = [ "case x of"
                     , "  y -> z"
                     , "   where z = x"]
      in shouldSubstituteTo subst e expected
    it "only considers where clauses for their respective case alternative"
      $ runTest
      $
      -- σ = { z ↦ x }
      let subst    = singleSubst (S.unQual z) (S.var x)
          e        = [ "case x of"
                     , "  x -> z"
                     , "   where z = x"
                     , "  y -> z"]
          expected = [ "case x of"
                     , "  x -> z"
                     , "   where z = x"
                     , "  y -> x"]
      in shouldSubstituteTo subst e expected
    it "does not rename variables bound later in the where clause"
      $ runTest
      $
      -- σ = { y ↦ x, z ↦ x }
      let subst    = substFromList [(S.unQual y, S.var x), (S.unQual z, S.var x)]
          e        = [ "case x of"
                     , "  x_0 -> x"
                     , "   where x = y"
                     , "         y = z"]
          expected = [ "case x of"
                     , "  x_0 -> x_0"
                     , "   where x_0 = y"
                     , "         y   = x"]
      in shouldSubstituteTo subst e expected
  context "substitution in modules" $ do
    it "avoids capture of variables shadowed in let expressions" $ runTest $
      let m        = [ "module A where"
                     , "f (x, y) = let x = y in x"]
          expected = [ "module A where"
                     , "f a0 = case a0 of"
                     , "  (a1, a2) -> let x = a2 in x"]
      in m `shouldTransformTo` expected
{- TODO The following test should be used instead of the replacement that is
        not commented out, after the corresponding issue has been fixed.
    it "avoids capture of variables shadowed in case expressions" $ runTest $
      let m        = [ "module A where"
                     , "f (x, y) = case x of"
                     , "             y -> (x, y)"]
          expected = [ "module A where"
                     , "f a0 = case a0 of"
                     , "  (a1, a2) -> case a1 of"
                     , "    y -> (a1, y)"]
      in m `shouldTransformTo` expected -}
    it "avoids capture of variables shadowed in case expressions" $ runTest $
      let m        = [ "module A where"
                     , "f (x, y) ="
                     , "  let r = case x of"
                     , "            y -> (x, y)"
                     , "  in  r"]
          expected = [ "module A where"
                     , "f a1 = case a1 of"
                     , "  (a2, a3) ->"
                     , "    let r = case a2 of"
                     , "              a0 -> (a2, a0)"
                     , "    in  r"]
      in m `shouldTransformTo` expected
