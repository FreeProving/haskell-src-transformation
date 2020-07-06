-- | This module contains basic tests for 'haskell-source-transformations'
module HST.ApplicationTests
  ( applicationTests
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

-- | Test group for basic unit tests
applicationTests :: Spec
applicationTests = describe "Basic unit tests" tests

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
      $ unlines [ "module A where"
                , "lengthL :: [a] -> Int"
                , "lengthL [] = 0"
                , "lengthL(_:xs) = 1 + lengthL xs"
                ]
    expected <-
      parseTestModule
      $  unlines [ "module A where"
                 , "lengthL :: [a] -> Int"
                 , "lengthL a0 = case a0 of"
                 , "  []    -> 0"
                 , "  a1:a2 -> 1 + lengthL a2"
                 ]
    let mod2 = evalPM (processModule mod1) defaultState
    expected `prettyShouldBe` mod2
  it "should transform pattern matching in a partial function" $ do
    mod1 <-
      parseTestModule
      $ unlines [ "module A where"
                , "head :: [a] -> a"
                , "head (x:xs) = x"
                ]
    let mod2 = evalPM (processModule mod1) defaultState
    expected <-
      parseTestModule
      $ unlines [ "module A where"
                , "head :: [a] -> a"
                , "head a0 = case a0 of"
                , "  a1 : a2 -> a1"
                , "  [] -> undefined"
                ]
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
      $ unlines [ "module A where\n"
                , "id :: a -> a\n"
                , "id a0 = let a2 = undefined"
                , "            a1 = case a0 of"
                , "              a3 -> if otherwise then a3"
                , "                    else a2"
                , "         in a1" ]
    mod2 `prettyShouldBe` expected
  it "should accept a more complex guarded function" $ do
    mod1 <-
      parseTestModule
      $  unlines [ "module A where\n"
                 , "useless :: (a -> Bool) -> a -> a -> a"
                 , "useless p x y | p x       = x"
                 , "              | otherwise = y"
                 ]
    let mod2 = evalPM (processModule mod1) defaultState
    expected <-
      parseTestModule
      $ unlines
          [ "module A where\n"
          , "useless :: (a -> Bool) -> a -> a -> a"
          , "useless a0 a1 a2 = let"
          , "                     a4 = undefined"
          , "                     a3 = case a0 of"
          , "                       a5 -> case a1 of"
          , "                        a6 -> case a2 of"
          , "                         a7 -> if a5 a6 then a6"
          , "                               else if otherwise then a7"
          , "                                    else a4"
          , "                   in a3"
          ]
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
