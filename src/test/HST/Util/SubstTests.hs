-- | This module contains tests for "HST.Util.Subst".
module HST.Util.SubstTests where

import           Polysemy                  ( Members, Sem )
import           Test.Hspec                ( Spec, context, describe, it )

import           HST.Effect.Cancel         ( Cancel )
import           HST.Effect.Report         ( Report )
import           HST.Effect.SetExpectation ( SetExpectation )
import           HST.Effect.WithFrontend   ( WithFrontend )
import qualified HST.Frontend.Syntax       as S
import           HST.Test.Expectation
  ( prettyExpShouldBe )
import           HST.Test.Parser           ( parseTestExp )
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
  inputExp <- parseTestExp input
  let outputExp = applySubst subst inputExp
  expectedOutputExp <- parseTestExp expectedOutput
  outputExp `prettyExpShouldBe` expectedOutputExp

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
      -- e    = \x -> (x, y)
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
    it "does not substitute bound variables in let expressions"
      $ runTest
      $
      -- σ    = { x ↦ z, y ↦ z }
      -- e    = let x = z in x y
      -- σ(e) = let x = z in x z
      let subst
            = substFromList [(S.unQual x, S.var z), (S.unQual y, S.var z)]
          e        = ["let x = z in x y"]
          expected = ["let x = z in x z"]
      in shouldSubstituteTo subst e expected
    it "renames variables that are no longer bound"
      $ runTest
      $
      -- σ    = { y ↦ z }
      -- e    = let x = let y = z in y
      --        in x z
      -- σ(e) = let x = let y = z in y
      --        in x y
      let subst    = singleSubst (S.unQual y) (S.var z)
          e        = ["let x = let y = z in y", "in x y"]
          expected = ["let x = let y = z in y", "in x z"]
      in shouldSubstituteTo subst e expected
    it "does not substitute bound variables in nested let expressions"
      $ runTest
      $
      -- σ    = { x ↦ z, y ↦ z }
      -- e    = let x = y
      --        in let y = z in x y
      -- σ(e) = let x = z
      --        in let y = z in x y
      let subst
            = substFromList [(S.unQual x, S.var z), (S.unQual y, S.var z)]
          e        = ["let x = y", "in let y = z in x y"]
          expected = ["let x = z", "in let y = z in x y"]
      in shouldSubstituteTo subst e expected
    it "does not use more variable names than needed"
      $ runTest
      $
      -- σ    = { y ↦ x }
      -- e    = let x = y
      --        in let x = y in x
      -- σ(e) = let x_0 = x
      --        in let x_0 = x in x_0
      let subst    = singleSubst (S.unQual y) (S.var x)
          e        = ["let x = y", "in let x = y in x"]
          expected = ["let x_0 = x", "in let x_0 = x in x_0"]
      in shouldSubstituteTo subst e expected
    it "avoids capture by renaming variables in let expressions"
      $ runTest
      $
      -- σ    = { y ↦ x, z ↦ x }
      -- e    = let x   = y
      --            x_0 = z
      --        in  x_1
      -- σ(e) = let x_0 = x
      --            x_2 = x
      --        in  x_1
      let subst
            = substFromList [(S.unQual y, S.var x), (S.unQual z, S.var x)]
          e        = ["let x   = y", "    x_0 = z", "in  x_1"]
          expected = ["let x_0 = x", "    x_2 = x", "in  x_1"]
      in shouldSubstituteTo subst e expected
    it "only substitutes variables not bound by a case alternative pattern"
      $ runTest
      $
      -- σ    = { x ↦ z, y ↦ z }
      -- e    = case x y of
      --          (x, x_0) -> (x, y)
      -- σ(e) = case z z of
      --          (x, x_0) -> (x, z)
      let subst
            = substFromList [(S.unQual x, S.var z), (S.unQual y, S.var z)]
          e        = ["case x y of", "  (x, x_0) -> (x, y)"]
          expected = ["case z z of", "  (x, x_0) -> (x, z)"]
      in shouldSubstituteTo subst e expected
    it "uses different name spaces for each case alternative"
      $ runTest
      $
      -- σ    = { x ↦ y }
      -- e    = case x of
      --          [y]   -> x
      --          y : z -> y : x
      -- σ(e) = case y of
      --          [y_0]   -> y
      --          y_0 : z -> y_0 : y
      let subst    = singleSubst (S.unQual x) (S.var y)
          e        = ["case x of", "  [y]   -> x", "  y : z -> y : x"]
          expected = ["case y of", "  [y_0]   -> y", "  y_0 : z -> y_0 : y"]
      in shouldSubstituteTo subst e expected
    it ("only renames bound variables in case alternatives where the free "
        ++ "variables they capture are used")
      $ runTest
      $
      -- σ    = { x ↦ y }
      -- e    = case x of
      --          y -> x y
      --          y -> let x = z in x y
      -- σ(e) = case y of
      --          y_0 -> y y_0
      --          y   -> let x = z in x y
      let subst    = singleSubst (S.unQual x) (S.var y)
          e        = ["case x of", "  y -> x y", "  y -> let x = z in x y"]
          expected
            = ["case y of", "  y_0 -> y y_0", "  y   -> let x = z in x y"]
      in shouldSubstituteTo subst e expected
    it "avoids capture by renaming variables in case alternative patterns"
      $ runTest
      $
      -- σ    = { y ↦ x, z ↦ x }
      -- e    = case [y, z] of
      --          x : x_2 : x_0 -> (x, y, z)
      -- σ(e) = case [x, x] of
      --          x_0 : x_2 : x_1 -> (x_0, x, x)
      let subst
            = substFromList [(S.unQual y, S.var x), (S.unQual z, S.var x)]
          e        = ["case [y, z] of", "  x : x_2 : x_0 -> (x, y, z)"]
          expected = ["case [x, x] of", "  x_0 : x_2 : x_1 -> (x_0, x, x)"]
      in shouldSubstituteTo subst e expected
    it "does not rename variables bound in where clauses"
      $ runTest
      $
      -- σ    = { z ↦ x }
      -- e    = case x of
      --          y -> z
      --           where z = x
      -- σ(e) = case x of
      --          y -> z
      --           where z = x
      let subst    = singleSubst (S.unQual z) (S.var x)
          e        = ["case x of", "  y -> z", "   where z = x"]
          expected = ["case x of", "  y -> z", "   where z = x"]
      in shouldSubstituteTo subst e expected
    it "only considers where clauses for their respective case alternative"
      $ runTest
      $
      -- σ    = { z ↦ x }
      -- e    = case x of
      --          x -> z
      --           where z = x
      --          y -> z
      -- σ(e) = case x of
      --          x -> z
      --           where z = x
      --          y -> x
      let subst    = singleSubst (S.unQual z) (S.var x)
          e        = ["case x of", "  x -> z", "   where z = x", "  y -> z"]
          expected = ["case x of", "  x -> z", "   where z = x", "  y -> x"]
      in shouldSubstituteTo subst e expected
    it "does not rename variables bound later in the where clause"
      $ runTest
      $
      -- σ    = { y ↦ x, z ↦ x }
      -- e    = case x of
      --          x_0 -> x
      --           where x = y
      --                 y = z
      -- σ(e) = case x of
      --          x_0 -> x_0
      --           where x_0 = y
      --                 y   = x
      let subst
            = substFromList [(S.unQual y, S.var x), (S.unQual z, S.var x)]
          e
            = ["case x of", "  x_0 -> x", "   where x = y", "         y = z"]
          expected = [ "case x of"
                     , "  x_0 -> x_0"
                     , "   where x_0 = y"
                     , "         y   = x"
                     ]
      in shouldSubstituteTo subst e expected
