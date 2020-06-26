module BasicTests ( basicTests ) where

import Test.Hspec

import Control.Monad ( void )

import Language.Haskell.Exts
import           Test.HUnit.Base                ( assertFailure )

import FreshVars
import Application ( processModule, specialCons )

basicTests :: Spec
basicTests = describe "Basic unit tests" tests

parseTestModule :: String -> IO (Module ())
parseTestModule modStr = case parseModule modStr of
  ParseOk modul        -> return $ void modul
  ParseFailed _ errMsg -> assertFailure errMsg

defaultState :: PMState
defaultState = PMState { nextId      = 0
                       , constrMap   = specialCons
                       , matchedPat  = []
                       , trivialCC   = False
                       , opt         = True
                       , debugOutput = ""
                       }


tests :: Spec
tests = do
  it "should accept a simple function" $ do
    mod1 <- parseTestModule "module A where\nf :: a -> a\nf x = x"
    let mod2 = evalPM (processModule mod1) defaultState
    if mod2 /= mod1 then assertFailure "Does not accept simple prog"
    else return ()
  it "should transform pattern matching into case expressions" $ do
    mod1 <- parseTestModule $ "module A where\nlengthL :: [a] -> Int\n"
                            ++ "lengthL [] = 0\nlengthL(_:xs) = 1 + lengthL xs"
    expected <- parseTestModule $ "module A where\nlengthL :: [a] -> Int\n"
                                ++ "lengthL a0 = case a0 of\n"
                                ++  "  []    -> 0\n"
                                ++  "  a1:a2 -> 1 + lengthL a2"
    let mod2 = evalPM (processModule mod1) defaultState
    if expected /= mod2 then assertFailure "does not transform pm correctly"
    else return ()
  it "should transform pattern matching in a partial function" $ do
    mod1 <- parseTestModule $  "module A where\n"
                            ++ "head :: [a] -> a\n"
                            ++ "head (x:xs) = x"
    let mod2 = evalPM (processModule mod1) defaultState
    expected <- parseTestModule $  "module A where\n"
                                ++ "head :: [a] -> a\n"
                                ++ "head a0 = case a0 of\n"
                                ++ "  a1 : a2 -> a1\n"
                                ++ "  [] -> undefined"
    if expected /= mod2 then assertFailure "does not transform partial fun"
    else return ()
