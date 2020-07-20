-- | This module contains basic tests for "HST.Application".
module HST.ApplicationTests
  ( testApplication
  )
where

import           Polysemy                       ( Member
                                                , Sem
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
import           HST.Effect.Fresh               ( Fresh
                                                , runFresh
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
runTest :: Sem '[Env a, Fresh, GetOpt, Report, Embed IO] b -> IO b
runTest = runM . reportToExpectation . runWithArgs [] . runFresh . runEnv

-------------------------------------------------------------------------------
-- Expectation Setters                                                       --
-------------------------------------------------------------------------------

-- | Parses the given modules, processes the input module with 'processModule'
--   and sets the expectation that the given output module is produced.
shouldTransformTo :: [String] -> [String] -> Expectation
shouldTransformTo input expectedOutput = do
  inputModule          <- parseTestModule (unlines input)
  expectedOutputModule <- parseTestModule (unlines expectedOutput)
  outputModule         <- runTest $ do
    inputModule'  <- FromHSE.transformModule inputModule
    outputModule' <- processModule inputModule'
    return (ToHSE.transformModule inputModule outputModule')
  outputModule `prettyShouldBe` expectedOutputModule

-- | Pretty prints both values and tests whether the resulting strings are
--   equal modulo whitespace.
prettyShouldBe :: (Pretty a, Pretty b) => a -> b -> Expectation
prettyShouldBe x y = prettyPrint x `shouldBe` prettyPrint y

-- | Handles the 'Report' effect by asserting that no fatal message is reported.
--
--   If there is a fatal message, all reported messages are included in
--   the error message.
reportToExpectation :: Member (Embed IO) r => Sem (Report ': r) a -> Sem r a
reportToExpectation comp = do
  (ms, mx) <- runReport comp
  case mx of
    Nothing -> embed $ assertFailure $ unlines
      ("The following messages were reported:" : map showPrettyMessage ms)
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
    shouldTransformTo ["module A where", "f :: a -> a", "f x = x"]
                      ["module A where", "f :: a -> a", "f x = x"]
  it "should transform pattern matching into case expressions" $ do
    shouldTransformTo
      [ "module A where"
      , "lengthL :: [a] -> Int"
      , "lengthL [] = 0"
      , "lengthL(_:xs) = 1 + lengthL xs"
      ]
      [ "module A where"
      , "lengthL :: [a] -> Int"
      , "lengthL a0 = case a0 of"
      , "  []    -> 0"
      , "  a1:a2 -> 1 + lengthL a2"
      ]
  it "should transform pattern matching in a partial function" $ do
    shouldTransformTo
      ["module A where", "head :: [a] -> a", "head (x:xs) = x"]
      [ "module A where"
      , "head :: [a] -> a"
      , "head a0 = case a0 of"
      , "  a1 : a2 -> a1"
      , "  [] -> undefined"
      ]
  it "should accept a simple guarded expression" $ do
    shouldTransformTo
      ["module A where", "id :: a -> a", "id x | otherwise = x"]
      [ "module A where"
      , "id :: a -> a"
      , "id a0 = let a1 = case a0 of"
      , "              a3 -> if otherwise then a3"
      , "                                 else a2"
      , "            a2 = undefined"
      , "        in a1"
      ]
  it "should accept a more complex guarded function" $ do
    shouldTransformTo
      [ "module A where"
      , "useless :: (a -> Bool) -> a -> a -> a"
      , "useless p x y | p x       = x"
      , "              | otherwise = y"
      ]
      [ "module A where"
      , "useless :: (a -> Bool) -> a -> a -> a"
      , "useless a0 a1 a2 ="
      , "  let a3 = case a0 of"
      , "        a5 -> case a1 of"
      , "          a6 -> case a2 of"
      , "            a7 -> if a5 a6 then a6"
      , "                           else if otherwise then a7"
      , "                                             else a4"
      , "      a4 = undefined"
      , "  in a3"
      ]
