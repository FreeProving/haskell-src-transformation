module HST.CoreAlgorithmTests
  ( testCoreAlgorithm
  )
where

import qualified Language.Haskell.Exts         as HSE
import           Test.Hspec                     ( Spec
                                                , Expectation
                                                , context
                                                , describe
                                                , it
                                                )
import           Test.HUnit.Base                ( assertFailure )

import           HST.CoreAlgorithm              ( compareCons )
import qualified HST.Frontend.FromHSE          as FromHSE

-- | Tests for the "HST.CoreAlgorithm" module.
testCoreAlgorithm :: Spec
testCoreAlgorithm = describe "HST.CoreAlgorithm" $ do
  testCompareCons

-- | Parse a pattern from the given string and sets the expectation that
--   parsing is successful.
parseTestPat :: String -> IO (HSE.Pat HSE.SrcSpanInfo)
parseTestPat patStr = case HSE.parsePat patStr of
  HSE.ParseOk pat          -> return pat
  HSE.ParseFailed _ errMsg -> assertFailure errMsg

-- | Sets the expectation that the given patterns should have matching
--   constructors.
shouldMatchCons
  :: HSE.Pat HSE.SrcSpanInfo -> HSE.Pat HSE.SrcSpanInfo -> Expectation
shouldMatchCons pat1 pat2
  | compareCons (FromHSE.transformPat pat1) (FromHSE.transformPat pat2)
  = return ()
  | otherwise
  = assertFailure
    $  "\""
    ++ HSE.prettyPrint pat1
    ++ "\" and \""
    ++ HSE.prettyPrint pat2
    ++ "\" should match the same constructor but they do not"

-- | Sets the expectation that the given patterns should not have matching
--   constructors.
shouldNotMatchCons
  :: HSE.Pat HSE.SrcSpanInfo -> HSE.Pat HSE.SrcSpanInfo -> Expectation
shouldNotMatchCons pat1 pat2
  | compareCons (FromHSE.transformPat pat1) (FromHSE.transformPat pat2)
  = assertFailure
    $  "\""
    ++ HSE.prettyPrint pat1
    ++ "\" and \""
    ++ HSE.prettyPrint pat2
    ++ "\" should not match the same constructor but they do"
  | otherwise
  = return ()

-- | Test group for 'compareCons' tests.
testCompareCons :: Spec
testCompareCons = context "matching constructors of patterns" $ do
  it "should match constructors in list notation and infix list constructor"
    $ do
        pat1 <- parseTestPat "[x]"
        pat2 <- parseTestPat "x : []"
        pat1 `shouldMatchCons` pat2
  it "should match constructors in list notation and prefix list constructor"
    $ do
        pat1 <- parseTestPat "[x]"
        pat2 <- parseTestPat "(:) x []"
        pat1 `shouldMatchCons` pat2
  it "should match constructors infix and prefix list constructor" $ do
    pat1 <- parseTestPat "x : []"
    pat2 <- parseTestPat "(:) x []"
    pat1 `shouldMatchCons` pat2
  it "should match constructors of non-empty lists with different lengths" $ do
    pat1 <- parseTestPat "[x]"
    pat2 <- parseTestPat "[x, y]"
    pat1 `shouldMatchCons` pat2
  it "should match constructors of two empty lists" $ do
    pat1 <- parseTestPat "[]"
    pat2 <- parseTestPat "[]"
    pat1 `shouldMatchCons` pat2
  it "should match constructors of tuple notation and pair constructor" $ do
    pat1 <- parseTestPat "(x, y)"
    pat2 <- parseTestPat "(,) x y"
    pat1 `shouldMatchCons` pat2
  it "should match constructors of tuple notation and triple constructor" $ do
    pat1 <- parseTestPat "(x, y, z)"
    pat2 <- parseTestPat "(,,) x y z"
    pat1 `shouldMatchCons` pat2
  it "should not match constructors on empty and non-empty lists" $ do
    pat1 <- parseTestPat "[]"
    pat2 <- parseTestPat "[x]"
    pat1 `shouldNotMatchCons` pat2
  it "should not match constructors on tuples of different lengths" $ do
    pat1 <- parseTestPat "(x, y)"
    pat2 <- parseTestPat "(x, y, z)"
    pat1 `shouldNotMatchCons` pat2
  it "should not match constructors with different names" $ do
    pat1 <- parseTestPat "C"
    pat2 <- parseTestPat "D"
    pat1 `shouldNotMatchCons` pat2
