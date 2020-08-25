-- | This module contains tests for "HST.Util.FreeVars".
module HST.Util.FreeVarsTests where

import           Polysemy                  ( Members, Sem )
import           Test.Hspec
  ( Spec, context, describe, it, shouldBe )

import           HST.Effect.Cancel         ( Cancel )
import           HST.Effect.Report         ( Report )
import           HST.Effect.SetExpectation ( SetExpectation, setExpectation )
import           HST.Effect.WithFrontend   ( WithFrontend )
import           HST.Frontend.Syntax       as S
import           HST.Test.Parser           ( parseTestModule )
import           HST.Test.Runner           ( runTest )
import           HST.Util.FreeVars         ( freeVars )

-------------------------------------------------------------------------------
-- Expectation Setters                                                       --
-------------------------------------------------------------------------------
-- | Parses the given Haskell module and sets the expectation that the given
--   variables are free.
--
--   All free
shouldBeFreeIn
  :: (Members '[Cancel, Report, SetExpectation, WithFrontend f] r, ShowAST f)
  => [String] -- ^ The identifiers of the expected free variables.
  -> [String] -- ^ The lines of the input module.
  -> Sem r ()
shouldBeFreeIn idents inputLines = do
  input <- parseTestModule inputLines
  let expectedFreeVars = map (S.unQual . S.Ident S.NoSrcSpan) idents
  setExpectation (freeVars input `shouldBe` expectedFreeVars)

-------------------------------------------------------------------------------
-- Tests                                                                     --
-------------------------------------------------------------------------------
-- | Test group for "HST.Util.FreeVars".
testFreeVars :: Spec
testFreeVars = describe "HST.Util.FreeVars" $ context "freeVars" $ do
  it "should find free variables" $ runTest $ ["g"] `shouldBeFreeIn` ["f = g"]
  it "should find free variables in left to right order" $ runTest $ do
    ["g", "x", "h"] `shouldBeFreeIn` ["f = (g x, h x)"]
  it "should find free variables in guards" $ runTest $ do
    ["c", "g"] `shouldBeFreeIn` ["f | c = g"]
  it "should find unbound variables of local declarations" $ runTest $ do
    ["h"] `shouldBeFreeIn` ["f = g where g = h"]
  it "should not contain arguments" $ runTest $ do
    [] `shouldBeFreeIn` ["id x = x"]
  it "should not contain local declarations" $ runTest $ do
    [] `shouldBeFreeIn` ["f = g where g = 42"]
  it "should not contain recursive calls" $ runTest $ do
    [] `shouldBeFreeIn` ["repeat x = x : repeat x"]
  it "should not contain lambda arguments" $ runTest $ do
    ["x"] `shouldBeFreeIn` ["f = \\g -> g x"]
  it "should not contain lambda arguments" $ runTest $ do
    ["x"] `shouldBeFreeIn` ["f = \\g -> g x"]
  it "should not contain lambda arguments" $ runTest $ do
    ["x"] `shouldBeFreeIn` ["f = \\g -> g x"]
  it "should not contain let-bindings" $ runTest $ do
    ["z", "g"] `shouldBeFreeIn` ["f = let {x = y; y = z} in g x"]
  it "should not contain patterns of case alternatives" $ runTest $ do
    ["g"] `shouldBeFreeIn` ["f xy = case xy of (x, y) -> g x y"]
  it "should contain free variables of guarded case alternatives" $ runTest $ do
    ["p", "g"] `shouldBeFreeIn` ["f xy = case xy of (x, y) | p x y -> g x y"]
