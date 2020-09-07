-- | This module contains tests for "HST.Util.Selectors".
module HST.Util.SelectorsTests where

import qualified Data.Set                  as Set
import           Test.Hspec

import           HST.Effect.SetExpectation ( setExpectation )
import           HST.Test.Parser           ( parseTestModule )
import           HST.Test.Runner           ( runTest )
import           HST.Util.Selectors        ( findIdentifiers )

-- | Test group for "HST.Util.Selectors".
testSelectors :: Spec
testSelectors = describe "HST.Util.Selectors" $ do
  testFindIdentifiers

-- | Test group for 'findIdentifiers' tests.
testFindIdentifiers :: Spec
testFindIdentifiers = context "findIdentifiers" $ do
  -- Function declarations.
  it "finds the name of function declarations" $ runTest $ do
    input <- parseTestModule ["f = ()"]
    setExpectation (findIdentifiers input `shouldBe` Set.fromList ["f"])
  it "finds identifiers on right-hand side of function declarations"
    $ runTest
    $ do
      input <- parseTestModule ["f = e"]
      setExpectation (findIdentifiers input `shouldBe` Set.fromList ["f", "e"])
  it "finds identifiers in guards of function declarations" $ runTest $ do
    input <- parseTestModule ["f | g = ()"]
    setExpectation (findIdentifiers input `shouldBe` Set.fromList ["f", "g"])
  it "finds names of local function declarations" $ runTest $ do
    input <- parseTestModule ["f = () where g = ()"]
    setExpectation (findIdentifiers input `shouldBe` Set.fromList ["f", "g"])
  -- Patterns.
  it "finds arguments of function declarations" $ runTest $ do
    input <- parseTestModule ["f x = ()"]
    setExpectation (findIdentifiers input `shouldBe` Set.fromList ["f", "x"])
  it "finds arguments in infix patterns" $ runTest $ do
    input <- parseTestModule ["f (x : xs) = ()"]
    setExpectation
      (findIdentifiers input `shouldBe` Set.fromList ["f", "x", "xs"])
  it "finds arguments in constructor patterns" $ runTest $ do
    input <- parseTestModule ["f (C x y) = ()"]
    setExpectation
      (findIdentifiers input `shouldBe` Set.fromList ["f", "C", "x", "y"])
  it "finds arguments in tuple patterns" $ runTest $ do
    input <- parseTestModule ["f (x, y) = ()"]
    setExpectation
      (findIdentifiers input `shouldBe` Set.fromList ["f", "x", "y"])
  it "finds arguments in list patterns" $ runTest $ do
    input <- parseTestModule ["f [x, y] = ()"]
    setExpectation
      (findIdentifiers input `shouldBe` Set.fromList ["f", "x", "y"])
  -- Expressions.
  it "finds identifiers in applications" $ runTest $ do
    input <- parseTestModule ["f = e1 e2"]
    setExpectation
      (findIdentifiers input `shouldBe` Set.fromList ["f", "e1", "e2"])
  it "finds identifiers in infix applications" $ runTest $ do
    input <- parseTestModule ["f = e1 + e2"]
    setExpectation
      (findIdentifiers input `shouldBe` Set.fromList ["f", "e1", "e2"])
  it "finds identifiers in infix applications" $ runTest $ do
    input <- parseTestModule ["f = e1 + e2"]
    setExpectation
      (findIdentifiers input `shouldBe` Set.fromList ["f", "e1", "e2"])
  it "finds identifiers in negation applications" $ runTest $ do
    input <- parseTestModule ["f = -e"]
    setExpectation (findIdentifiers input `shouldBe` Set.fromList ["f", "e"])
  it "finds identifiers in lambda abstractions" $ runTest $ do
    input <- parseTestModule ["f = \\x -> e"]
    setExpectation
      (findIdentifiers input `shouldBe` Set.fromList ["f", "x", "e"])
  it "finds identifiers in `let`-expression" $ runTest $ do
    input <- parseTestModule ["f = let x = e1 in e2"]
    setExpectation
      (findIdentifiers input `shouldBe` Set.fromList ["f", "x", "e1", "e2"])
  it "finds identifiers in `if`-expression" $ runTest $ do
    input <- parseTestModule ["f = if e1 then e2 else e3"]
    setExpectation
      (findIdentifiers input `shouldBe` Set.fromList ["f", "e1", "e2", "e3"])
  it "finds identifiers in `case`-expression" $ runTest $ do
    input <- parseTestModule ["f = case e1 of { C x -> e2 }"]
    setExpectation (findIdentifiers input
                    `shouldBe` Set.fromList ["f", "e1", "C", "x", "e2"])
  it "finds identifiers in guards of `case`-alternatives" $ runTest $ do
    input <- parseTestModule ["f = case () of { () | g -> () }"]
    setExpectation (findIdentifiers input `shouldBe` Set.fromList ["f", "g"])
  it "finds identifiers in `where`-clauses of `case`-alternatives" $ runTest $ do
    input <- parseTestModule ["f = case () of { () -> () where g = () }"]
    setExpectation (findIdentifiers input `shouldBe` Set.fromList ["f", "g"])
  it "finds identifiers in tuples" $ runTest $ do
    input <- parseTestModule ["f = (e1, e2)"]
    setExpectation
      (findIdentifiers input `shouldBe` Set.fromList ["f", "e1", "e2"])
  it "finds identifiers in list literals" $ runTest $ do
    input <- parseTestModule ["f = [e1, e2]"]
    setExpectation
      (findIdentifiers input `shouldBe` Set.fromList ["f", "e1", "e2"])
  it "finds identifiers in parentheses" $ runTest $ do
    input <- parseTestModule ["f = (e)"]
    setExpectation (findIdentifiers input `shouldBe` Set.fromList ["f", "e"])
  it "finds identifiers in expressions with type annotations" $ runTest $ do
    input <- parseTestModule ["f = e :: T"]
    setExpectation (findIdentifiers input `shouldBe` Set.fromList ["f", "e"])
