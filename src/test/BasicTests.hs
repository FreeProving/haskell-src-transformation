-- | This module contains basic tests for 'haskell-source-transformations'
module BasicTests
  ( basicTests
  )
where

import           Test.Hspec                     ( Spec
                                                , Expectation
                                                , describe
                                                , it
                                                , shouldBe
                                                )
import           Test.HUnit.Base                ( assertFailure )

import           Control.Monad                  ( void )

import           Language.Haskell.Exts          ( Module
                                                , ParseResult(..)
                                                , parseModule
                                                )
import           Language.Haskell.Exts.Pretty   ( Pretty
                                                , prettyPrint
                                                )

import           HST.Application                ( processModule
                                                , specialCons
                                                )
import           HST.Environment.FreshVars      ( PMState(..)
                                                , evalPM
                                                )


basicTests :: Spec
basicTests = describe "Basic unit tests" tests

-- | Parses a given string to a module and fails if parsing is not
--   successful.
parseTestModule :: String -> IO (Module ())
parseTestModule modStr = case parseModule modStr of
  ParseOk modul        -> return $ void modul
  ParseFailed _ errMsg -> assertFailure errMsg

-- | The default state used for pattern matching.
defaultState :: PMState
defaultState = PMState { nextId      = 0
                       , constrMap   = specialCons
                       , matchedPat  = []
                       , trivialCC   = False
                       , opt         = True
                       , debugOutput = ""
                       }

-- | Test cases for 'processModule'.
tests :: Spec
tests = do
  it "should accept a simple function" $ do
    mod1 <- parseTestModule "module A where\nf :: a -> a\nf x = x"
    let mod2 = evalPM (processModule mod1) defaultState
    mod2 `prettyShouldBe` mod1
  it "should transform pattern matching into case expressions" $ do
    mod1 <-
      parseTestModule
      $  "module A where\nlengthL :: [a] -> Int\n"
      ++ "lengthL [] = 0\nlengthL(_:xs) = 1 + lengthL xs"
    expected <-
      parseTestModule
      $  "module A where\nlengthL :: [a] -> Int\n"
      ++ "lengthL a0 = case a0 of\n"
      ++ "  []    -> 0\n"
      ++ "  a1:a2 -> 1 + lengthL a2"
    let mod2 = evalPM (processModule mod1) defaultState
    expected `prettyShouldBe` mod2
  it "should transform pattern matching in a partial function" $ do
    mod1 <-
      parseTestModule
      $  "module A where\n"
      ++ "head :: [a] -> a\n"
      ++ "head (x:xs) = x"
    let mod2 = evalPM (processModule mod1) defaultState
    expected <-
      parseTestModule
      $  "module A where\n"
      ++ "head :: [a] -> a\n"
      ++ "head a0 = case a0 of\n"
      ++ "  a1 : a2 -> a1\n"
      ++ "  [] -> undefined"
    mod2 `prettyShouldBe` expected
  it "should accept a simple guarded expression" $ do
    mod1 <-
      parseTestModule
      $  "module A where\n"
      ++ "id :: a -> a\n"
      ++ "id x | otherwise = x"
    let mod2 = evalPM (processModule mod1) defaultState
    expected <-
      parseTestModule
      $  "module A where\n"
      ++ "id :: a -> a\n"
      ++ "id a0 = let a2 = undefined\n"
      ++ "            a1 = case a0 of\n"
      ++ "              a3 -> if otherwise then a3"
      ++ "                    else a2\n"
      ++ "         in a1"
    mod2 `prettyShouldBe` expected
  it "should accept a more complex guarded function" $ do
    mod1 <-
      parseTestModule
      $  "module A where\n"
      ++ "useless :: (a -> Bool) -> a -> a -> a\n"
      ++ "useless p x y | p x       = x\n"
      ++ "              | otherwise = y"
    let mod2 = evalPM (processModule mod1) defaultState
    expected <-
      parseTestModule
      $  "module A where\n"
      ++ "useless :: (a -> Bool) -> a -> a -> a\n"
      ++ "useless a0 a1 a2 = let\n"
      ++ "                     a4 = undefined\n"
      ++ "                     a3 = case a0 of\n"
      ++ "                       a5 -> case a1 of\n"
      ++ "                        a6 -> case a2 of\n"
      ++ "                         a7 -> if a5 a6 then a6\n"
      ++ "                               else if otherwise then a7\n"
      ++ "                                    else a4\n"
      ++ "                   in a3"
    mod2 `prettyShouldBe` expected

-------------------------------------------------------------------------------
-- Pretty printing comparison                                                --
-------------------------------------------------------------------------------

-- | Pretty prints both values and tests whether the resulting strings are
--   equal modulo whitespace.
prettyShouldBe :: (Pretty a, Pretty b) => a -> b -> Expectation
prettyShouldBe x y =
  let discardWhitespace = unwords . words
      prettyX           = discardWhitespace (prettyPrint x)
      prettyY           = discardWhitespace (prettyPrint y)
  in  prettyX `shouldBe` prettyY
