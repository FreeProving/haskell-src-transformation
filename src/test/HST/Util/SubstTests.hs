-- | This module contains tests for "HST.Util.Subst".
--
--   TODO Implement expression parser.
--   The tests currently construct AST nodes manually since there is no parser
--   for expressions, yet.
module HST.Util.SubstTests where

import           Polysemy                  ( Member, Sem )
import           Test.Hspec
  ( Spec, context, describe, it, shouldBe )

import           HST.Effect.SetExpectation ( setExpectation )
import           HST.Effect.WithFrontend   ( WithFrontend )
import qualified HST.Frontend.Syntax       as S
import           HST.Test.Runner           ( runTest )
import           HST.Util.Subst
  ( applySubst, composeSubst, singleSubst, substFromList )

-------------------------------------------------------------------------------
-- Test Values                                                               --
-------------------------------------------------------------------------------
-- | Name of of a test function @f@.
f :: S.Name a
f = S.Ident S.NoSrcSpan "f"

-- | Name of of a test variable @x@.
x :: S.Name a
x = S.Ident S.NoSrcSpan "x"

-- | Name of of the fresh test variable @x_0@.
x_0 :: S.Name a
x_0 = S.Ident S.NoSrcSpan "x_0"

-- | Name of of the fresh test variable @x_1@.
x_1 :: S.Name a
x_1 = S.Ident S.NoSrcSpan "x_1"

-- | Name of of a test variable @y@.
y :: S.Name a
y = S.Ident S.NoSrcSpan "y"

-- | Name of of a test variable @z@.
z :: S.Name a
z = S.Ident S.NoSrcSpan "z"

-------------------------------------------------------------------------------
-- Utility Functions                                                         --
-------------------------------------------------------------------------------
-- | Helper function that disambiguates the front end configuration type
--   variable of an AST node.
--
--   Without this helper function the 'S.ShowAST' instance that is provided by
--   the 'WithFrontend' effect cannot be used by 'shouldBe' in the tests below.
currentFrontendSyntax :: Member (WithFrontend f) r => node f -> Sem r (node f)
currentFrontendSyntax = return

-------------------------------------------------------------------------------
-- Tests                                                                     --
-------------------------------------------------------------------------------
-- | Test group for "HST.Util.FreeVars".
testSubst :: Spec
testSubst = describe "HST.Util.Subst" $ do
  context "composeSubst" $ do
    it "applies the second substitution first" $ runTest $ do
      -- σ₁   = { x ↦ z }
      -- σ₂   = { x ↦ y }
      -- σ    = σ₁ . σ₂ = { x ↦ y }
      -- e    = x y z
      -- σ(e) = y y z
      let s1    = singleSubst (S.unQual x) (S.var z)
          s2    = singleSubst (S.unQual x) (S.var y)
          subst = composeSubst s1 s2
          e     = S.app S.NoSrcSpan (S.var x) [S.var y, S.var z]
      e' <- currentFrontendSyntax $ applySubst subst e
      expected <- currentFrontendSyntax
        $ S.app S.NoSrcSpan (S.var y) [S.var y, S.var z]
      setExpectation (e' `shouldBe` expected)
    it "applies the second substitution to the first" $ runTest $ do
      -- σ₁   = { y ↦ z }
      -- σ₂   = { x ↦ y }
      -- σ    = σ₁ . σ₂ = { x ↦ z, y ↦ z }
      -- e    = x y z
      -- σ(e) = z z z
      let s1    = singleSubst (S.unQual y) (S.var z)
          s2    = singleSubst (S.unQual x) (S.var y)
          subst = composeSubst s1 s2
          e     = S.app S.NoSrcSpan (S.var x) [S.var y, S.var z]
      e' <- currentFrontendSyntax $ applySubst subst e
      expected <- currentFrontendSyntax
        $ S.app S.NoSrcSpan (S.var z) [S.var z, S.var z]
      setExpectation (e' `shouldBe` expected)
  context "applySubst" $ do
    it "substitutes variables" $ runTest $ do
      -- σ    = { x ↦ y }
      -- e    = f x
      -- σ(e) = f y
      let subst = singleSubst (S.unQual x) (S.var y)
          e     = S.App S.NoSrcSpan (S.var f) (S.var x)
      e' <- currentFrontendSyntax $ applySubst subst e
      expected <- currentFrontendSyntax $ S.App S.NoSrcSpan (S.var f) (S.var y)
      setExpectation (e' `shouldBe` expected)
    it "does not substitute bound variables" $ runTest $ do
      -- σ    = { x ↦ z, y ↦ z }
      -- e    = (\x -> f x y, x, y)
      -- σ(e) = (\x -> f x z, z, z)
      let subst = substFromList [(S.unQual x, S.var z), (S.unQual y, S.var z)]
          e     = S.Tuple S.NoSrcSpan S.Boxed
            [ S.Lambda S.NoSrcSpan [S.PVar S.NoSrcSpan x]
                (S.app S.NoSrcSpan (S.var f) [S.var x, S.var y])
            , S.var x
            , S.var y
            ]
      e' <- currentFrontendSyntax $ applySubst subst e
      expected <- currentFrontendSyntax
        $ S.Tuple S.NoSrcSpan S.Boxed
        [ S.Lambda S.NoSrcSpan [S.PVar S.NoSrcSpan x]
            (S.app S.NoSrcSpan (S.var f) [S.var x, S.var z])
        , S.var z
        , S.var z
        ]
      setExpectation (e' `shouldBe` expected)
    it "renames bound variables to avoid capture" $ runTest $ do
      -- σ    = { y ↦ x }
      -- e    = \x -> (x, y)
      -- σ(e) = \x_0 -> (x_0, x)
      let subst = singleSubst (S.unQual y) (S.var x)
          e     = S.Lambda S.NoSrcSpan [S.PVar S.NoSrcSpan x]
            (S.Tuple S.NoSrcSpan S.Boxed [S.var x, S.var y])
      e' <- currentFrontendSyntax $ applySubst subst e
      expected <- currentFrontendSyntax
        $ S.Lambda S.NoSrcSpan [S.PVar S.NoSrcSpan x_0]
        (S.Tuple S.NoSrcSpan S.Boxed [S.var x_0, S.var x])
      setExpectation (e' `shouldBe` expected)
    it "renames bound variables only if necessary" $ runTest $ do
      -- σ    = { y ↦ z }
      -- e    = \x -> (x, y)
      -- σ(e) = \x -> (x, z)
      let subst = singleSubst (S.unQual y) (S.var z)
          e     = S.Lambda S.NoSrcSpan [S.PVar S.NoSrcSpan x]
            (S.Tuple S.NoSrcSpan S.Boxed [S.var x, S.var y])
      e' <- currentFrontendSyntax $ applySubst subst e
      expected <- currentFrontendSyntax
        $ S.Lambda S.NoSrcSpan [S.PVar S.NoSrcSpan x]
        (S.Tuple S.NoSrcSpan S.Boxed [S.var x, S.var z])
      setExpectation (e' `shouldBe` expected)
    it "avoids capture of renamed variables" $ runTest $ do
      -- σ    = { y ↦ x }
      -- e    = \x   x_0 -> (x, x_0, y)
      -- σ(e) = \x_0 x_1 -> (x_0, x_1, x)
      let subst = singleSubst (S.unQual y) (S.var x)
          e     = S.Lambda S.NoSrcSpan
            [S.PVar S.NoSrcSpan x, S.PVar S.NoSrcSpan x_0]
            (S.Tuple S.NoSrcSpan S.Boxed [S.var x, S.var x_0, S.var y])
      e' <- currentFrontendSyntax $ applySubst subst e
      expected <- currentFrontendSyntax
        $ S.Lambda S.NoSrcSpan [S.PVar S.NoSrcSpan x_0, S.PVar S.NoSrcSpan x_1]
        (S.Tuple S.NoSrcSpan S.Boxed [S.var x_0, S.var x_1, S.var x])
      setExpectation (e' `shouldBe` expected)
