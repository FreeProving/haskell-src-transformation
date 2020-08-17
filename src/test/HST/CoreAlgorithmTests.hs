module HST.CoreAlgorithmTests ( testCoreAlgorithm ) where

import qualified Language.Haskell.Exts as HSE
import           Polysemy ( Member, Sem, runM )
import           Polysemy.Embed ( Embed, embed )
import           Test.HUnit.Base ( assertFailure )
import           Test.Hspec ( Expectation, Spec, context, describe, it )

import           HST.CoreAlgorithm ( compareCons )
import           HST.Effect.Report
import           HST.Frontend.HSE.Config ( HSE )
import qualified HST.Frontend.HSE.From as FromHSE
import qualified HST.Frontend.HSE.To as ToHSE
import qualified HST.Frontend.Syntax as S

-- | Tests for the "HST.CoreAlgorithm" module.
testCoreAlgorithm :: Spec
testCoreAlgorithm = describe "HST.CoreAlgorithm"
  $ do
    testCompareCons

-- | Parse a pattern from the given string and sets the expectation that
--   parsing is successful.
parseTestPat :: String -> IO (S.Pat HSE)
parseTestPat patStr = case HSE.parsePat patStr of
  HSE.ParseOk pat          ->
    runM . reportToExpectation $ FromHSE.transformPat pat
  HSE.ParseFailed _ errMsg -> assertFailure errMsg

-- | Handles the 'Report' effect by asserting that no fatal message is reported.
--
--   If there is a fatal message, all reported messages are included in
--   the error message.
reportToExpectation :: Member (Embed IO) r => Sem (Report ': r) a -> Sem r a
reportToExpectation comp = do
  (ms, mx) <- runReport comp
  case mx of
    Nothing -> embed
      $ assertFailure
      $ unlines
      ("The following messages were reported:" : map showPrettyMessage ms)
    Just x  -> return x

-- | Sets the expectation that the given patterns should have matching
--   constructors.
shouldMatchCons :: S.Pat HSE -> S.Pat HSE -> Expectation
shouldMatchCons pat1 pat2
  | compareCons pat1 pat2 = return ()
  | otherwise = assertFailure
    $ "\""
    ++ HSE.prettyPrint (ToHSE.transformPat pat1)
    ++ "\" and \""
    ++ HSE.prettyPrint (ToHSE.transformPat pat2)
    ++ "\" should match the same constructor but they do not"

-- | Sets the expectation that the given patterns should not have matching
--   constructors.
shouldNotMatchCons :: S.Pat HSE -> S.Pat HSE -> Expectation
shouldNotMatchCons pat1 pat2
  | compareCons pat1 pat2 = assertFailure
    $ "\""
    ++ HSE.prettyPrint (ToHSE.transformPat pat1)
    ++ "\" and \""
    ++ HSE.prettyPrint (ToHSE.transformPat pat2)
    ++ "\" should not match the same constructor but they do"
  | otherwise = return ()

-- | Test group for 'compareCons' tests.
testCompareCons :: Spec
testCompareCons = context "matching constructors of patterns"
  $ do
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
    it "should match constructors infix and prefix list constructor"
      $ do
        pat1 <- parseTestPat "x : []"
        pat2 <- parseTestPat "(:) x []"
        pat1 `shouldMatchCons` pat2
    it "should match constructors of non-empty lists with different lengths"
      $ do
        pat1 <- parseTestPat "[x]"
        pat2 <- parseTestPat "[x, y]"
        pat1 `shouldMatchCons` pat2
    it "should match constructors of two empty lists"
      $ do
        pat1 <- parseTestPat "[]"
        pat2 <- parseTestPat "[]"
        pat1 `shouldMatchCons` pat2
    it "should match constructors of tuple notation and pair constructor"
      $ do
        pat1 <- parseTestPat "(x, y)"
        pat2 <- parseTestPat "(,) x y"
        pat1 `shouldMatchCons` pat2
    it "should match constructors of tuple notation and triple constructor"
      $ do
        pat1 <- parseTestPat "(x, y, z)"
        pat2 <- parseTestPat "(,,) x y z"
        pat1 `shouldMatchCons` pat2
    it "should not match constructors on empty and non-empty lists"
      $ do
        pat1 <- parseTestPat "[]"
        pat2 <- parseTestPat "[x]"
        pat1 `shouldNotMatchCons` pat2
    it "should not match constructors on tuples of different lengths"
      $ do
        pat1 <- parseTestPat "(x, y)"
        pat2 <- parseTestPat "(x, y, z)"
        pat1 `shouldNotMatchCons` pat2
    it "should not match constructors with different names"
      $ do
        pat1 <- parseTestPat "C"
        pat2 <- parseTestPat "D"
        pat1 `shouldNotMatchCons` pat2
