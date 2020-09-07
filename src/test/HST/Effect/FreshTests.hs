-- | This module contains tests for "HST.Effect.Fresh".
module HST.Effect.FreshTests ( testFreshEffect ) where

import qualified Data.Set                  as Set
import           Polysemy                  ( Sem, runM )
import           Polysemy.Embed            ( Embed )
import           Test.Hspec
  ( Spec, context, describe, it, shouldBe )

import           HST.Effect.Fresh          ( Fresh, freshIdent, runFresh )
import           HST.Effect.SetExpectation
  ( SetExpectation, setExpectation, setExpectationToIO )

-------------------------------------------------------------------------------
-- Utility Functions                                                         --
-------------------------------------------------------------------------------
-- | Runs the given test with the 'runFresh' handler.
--
--   The given list of identifiers is passed to the 'runFresh' handler as a
--   set of used identifiers,
runFreshTest :: [String] -> Sem '[Fresh, SetExpectation, Embed IO] a -> IO a
runFreshTest usedIdents
  = runM . setExpectationToIO . runFresh (Set.fromList usedIdents)

-------------------------------------------------------------------------------
-- Tests                                                                     --
-------------------------------------------------------------------------------
-- | Test group for interpreters of the 'HST.Effect.Fresh.Fresh' effect.
testFreshEffect :: Spec
testFreshEffect = describe "HST.Effect.Fresh" $ do
  testRunFresh

-- | Test group for 'runFresh' tests.
testRunFresh :: Spec
testRunFresh = context "runFresh" $ do
  it "starts counting at zero" $ runFreshTest [] $ do
    x0 <- freshIdent "x"
    setExpectation (x0 `shouldBe` "x0")
  it "uses next index for subsequent fresh variables" $ runFreshTest [] $ do
    _ <- freshIdent "x"
    x1 <- freshIdent "x"
    setExpectation (x1 `shouldBe` "x1")
  it "has a different counter for each prefix" $ runFreshTest [] $ do
    x0 <- freshIdent "x"
    y0 <- freshIdent "y"
    x1 <- freshIdent "x"
    y1 <- freshIdent "y"
    setExpectation (x0 `shouldBe` "x0")
    setExpectation (y0 `shouldBe` "y0")
    setExpectation (x1 `shouldBe` "x1")
    setExpectation (y1 `shouldBe` "y1")
  it "skips used identifiers" $ runFreshTest ["x0"] $ do
    x1 <- freshIdent "x"
    setExpectation (x1 `shouldBe` "x1")
  it "does not skip unused identifiers" $ runFreshTest ["y"] $ do
    x0 <- freshIdent "x"
    setExpectation (x0 `shouldBe` "x0")
  it "skips multiple used identifiers" $ runFreshTest ["x0", "x1"] $ do
    x2 <- freshIdent "x"
    setExpectation (x2 `shouldBe` "x2")
  it "remembers last index when skipping identifiers" $ runFreshTest ["x0"] $ do
    _ <- freshIdent "x"
    x2 <- freshIdent "x"
    setExpectation (x2 `shouldBe` "x2")
  it "remembers last index when skipping multiple identifiers"
    $ runFreshTest ["x0", "x1"]
    $ do
      _ <- freshIdent "x"
      x3 <- freshIdent "x"
      setExpectation (x3 `shouldBe` "x3")
  it "skips used identifiers that are not the first fresh identifier"
    $ runFreshTest ["x1"]
    $ do
      x0 <- freshIdent "x"
      x2 <- freshIdent "x"
      setExpectation (x0 `shouldBe` "x0")
      setExpectation (x2 `shouldBe` "x2")
