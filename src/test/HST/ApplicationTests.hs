-- | This module contains basic tests for "HST.Application".
module HST.ApplicationTests
  ( testApplication
  )
where

import           Polysemy                       ( Member
                                                , Sem
                                                , raiseUnder
                                                , runM
                                                )
import           Polysemy.Embed                 ( Embed
                                                , embed
                                                )
import           Test.Hspec                     ( Spec
                                                , Expectation
                                                , context
                                                , describe
                                                , it
                                                , shouldBe
                                                )
import           Test.HUnit.Base                ( assertFailure )

import           Language.Haskell.Exts          ( Module
                                                , ParseResult(..)
                                                , SrcSpanInfo
                                                , parseModule
                                                )
import           Language.Haskell.Exts.Pretty   ( Pretty
                                                , prettyPrint
                                                )

import           HST.Application                ( processModule )
import           HST.Effect.Env                 ( Env
                                                , runEnv
                                                )
import           HST.Effect.GetOpt              ( GetOpt
                                                , runWithArgs
                                                )
import           HST.Effect.Report              ( Report
                                                , runReport
                                                , showPrettyMessage
                                                )
import qualified HST.Frontend.FromHSE          as FromHSE
import qualified HST.Frontend.ToHSE            as ToHSE

-------------------------------------------------------------------------------
-- Utility Functions                                                         --
-------------------------------------------------------------------------------

-- | Parses a given string to a module and fails if parsing is not
--   successful.
parseTestModule :: String -> IO (Module SrcSpanInfo)
parseTestModule modStr = case parseModule modStr of
  ParseOk modul        -> return modul
  ParseFailed _ errMsg -> assertFailure errMsg

-- | Runs the given computation with an empty environment and no additional
--   command line arguments.
runTest :: Sem '[Env a, GetOpt, Embed IO] b -> IO b
runTest = runM . reportToExpectation . runWithArgs [] . raiseUnder . runEnv

-------------------------------------------------------------------------------
-- Expectation Setters                                                       --
-------------------------------------------------------------------------------

-- | Pretty prints both values and tests whether the resulting strings are
--   equal modulo whitespace.
prettyShouldBe :: (Pretty a, Pretty b) => a -> b -> Expectation
prettyShouldBe x y = prettyPrint x `shouldBe` prettyPrint y

reportToExpectation :: Member (Embed IO) r => Sem (Report ': r) a -> Sem r a
reportToExpectation comp = do
  (ms, mx) <- runReport comp
  case mx of
    Nothing -> embed
      (assertFailure
        (unlines
          ("The following messages were reported:" : map showPrettyMessage ms)
        )
      )
    Just x -> return x

-------------------------------------------------------------------------------
-- Tests                                                                     --
-------------------------------------------------------------------------------

-- | Tests for the "HST.Application" module.
testApplication :: Spec
testApplication = describe "HST.Application" $ do
  testProcessModule

-- | Test cases for 'processModule'.
testProcessModule :: Spec
testProcessModule = context "processModule" $ do
  it "should accept a simple function" $ do
    mod1 <- parseTestModule
      $ unlines ["module A where", "f :: a -> a", "f x = x"]
    mod2' <- runTest (processModule (FromHSE.transformModule mod1))
    let mod2 = ToHSE.transformModule mod1 mod2'
    mod2 `prettyShouldBe` mod1
  it "should transform pattern matching into case expressions" $ do
    mod1 <- parseTestModule $ unlines
      [ "module A where"
      , "lengthL :: [a] -> Int"
      , "lengthL [] = 0"
      , "lengthL(_:xs) = 1 + lengthL xs"
      ]
    expected <- parseTestModule $ unlines
      [ "module A where"
      , "lengthL :: [a] -> Int"
      , "lengthL a0 = case a0 of"
      , "  []    -> 0"
      , "  a1:a2 -> 1 + lengthL a2"
      ]
    mod2' <- runTest (processModule (FromHSE.transformModule mod1))
    let mod2 = ToHSE.transformModule mod1 mod2'
    expected `prettyShouldBe` mod2
  it "should transform pattern matching in a partial function" $ do
    mod1 <- parseTestModule
      $ unlines ["module A where", "head :: [a] -> a", "head (x:xs) = x"]
    mod2' <- runTest (processModule (FromHSE.transformModule mod1))
    let mod2 = ToHSE.transformModule mod1 mod2'
    expected <- parseTestModule $ unlines
      [ "module A where"
      , "head :: [a] -> a"
      , "head a0 = case a0 of"
      , "  a1 : a2 -> a1"
      , "  [] -> undefined"
      ]
    mod2 `prettyShouldBe` expected
  it "should accept a simple guarded expression" $ do
    mod1 <- parseTestModule
      $ unlines ["module A where", "id :: a -> a", "id x | otherwise = x"]
    mod2' <- runTest (processModule (FromHSE.transformModule mod1))
    let mod2 = ToHSE.transformModule mod1 mod2'
    expected <- parseTestModule $ unlines
      [ "module A where"
      , "id :: a -> a"
      , "id a0 = let a2 = undefined"
      , "            a1 = case a0 of"
      , "              a3 -> if otherwise then a3"
      , "                                 else a2"
      , "         in a1"
      ]
    mod2 `prettyShouldBe` expected
  it "should accept a more complex guarded function" $ do
    mod1 <- parseTestModule $ unlines
      [ "module A where"
      , "useless :: (a -> Bool) -> a -> a -> a"
      , "useless p x y | p x       = x"
      , "              | otherwise = y"
      ]
    mod2' <- runTest (processModule (FromHSE.transformModule mod1))
    let mod2 = ToHSE.transformModule mod1 mod2'
    expected <- parseTestModule $ unlines
      [ "module A where"
      , "useless :: (a -> Bool) -> a -> a -> a"
      , "useless a0 a1 a2 ="
      , "  let a4 = undefined"
      , "      a3 = case a0 of"
      , "        a5 -> case a1 of"
      , "          a6 -> case a2 of"
      , "            a7 -> if a5 a6 then a6"
      , "                           else if otherwise then a7"
      , "                                             else a4"
      , "  in a3"
      ]
    mod2 `prettyShouldBe` expected
