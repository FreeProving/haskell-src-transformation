{-# LANGUAGE RankNTypes #-}

-- | This module contains basic tests for "HST.Application".
module HST.ApplicationTests ( testApplication ) where

import           Polysemy                  ( Members, Sem )
import           Test.Hspec                ( Spec, context, describe, it )

import           HST.Application           ( processModule )
import           HST.Effect.Cancel         ( Cancel )
import           HST.Effect.Env            ( runEnv )
import           HST.Effect.Fresh          ( runFresh )
import           HST.Effect.GetOpt         ( GetOpt )
import           HST.Effect.Report         ( Report )
import           HST.Effect.SetExpectation ( SetExpectation )
import           HST.Effect.WithFrontend   ( WithFrontend )
import qualified HST.Frontend.Syntax       as S
import           HST.Test.Expectation      ( prettyModuleShouldBe )
import           HST.Test.Parser           ( parseTestModule )
import           HST.Test.Runner           ( runTest )

-------------------------------------------------------------------------------
-- Expectation Setters                                                       --
-------------------------------------------------------------------------------
-- | Parses the given modules, processes the input module with 'processModule'
--   and sets the expectation that the given output module is produced.
shouldTransformTo
  :: ( S.EqAST f
     , Members '[GetOpt, Cancel, Report, SetExpectation, WithFrontend f] r
     )
  => [String]
  -> [String]
  -> Sem r ()
shouldTransformTo input expectedOutput = do
  inputModule <- parseTestModule input
  outputModule <- runEnv . runFresh $ processModule inputModule
  expectedOutputModule <- parseTestModule expectedOutput
  outputModule `prettyModuleShouldBe` expectedOutputModule

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
  it "should leave functions without pattern matching unchanged"
    $ runTest
    $ shouldTransformTo ["module A where", "f :: a -> a", "f x = x"]
    ["module A where", "f :: a -> a", "f x = x"]
  it "should transform pattern matching into case expressions"
    $ runTest
    $ shouldTransformTo [ "module A where"
                        , "lengthL :: [a] -> Int"
                        , "lengthL []       = 0"
                        , "lengthL (_ : xs) = 1 + lengthL xs"
                        ]
    [ "module A where"
    , "lengthL :: [a] -> Int"
    , "lengthL a0 = case a0 of"
    , "  []      -> 0"
    , "  a1 : a2 -> 1 + lengthL a2"
    ]
  it "should transform pattern matching in a partial function"
    $ runTest
    $ shouldTransformTo
    ["module A where", "head :: [a] -> a", "head (x:xs) = x"]
    [ "module A where"
    , "head :: [a] -> a"
    , "head a0 = case a0 of"
    , "  a1 : a2 -> a1"
    , "  []      -> undefined"
    ]
  it "should accept a simple guarded expression"
    $ runTest
    $ shouldTransformTo
    ["module A where", "id :: a -> a", "id x | otherwise = x"]
    [ "module A where"
    , "id :: a -> a"
    , "id a0 ="
    , "  let a1 = case a0 of"
    , "        a3 -> if otherwise then a3 else a2"
    , "      a2 = undefined"
    , "  in  a1"
    ]
  it "should accept a more complex guarded function"
    $ runTest
    $ shouldTransformTo [ "module A where"
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
    , "            a7 -> if a5 a6 then a6 else if otherwise then a7 else a4"
    , "      a4 = undefined"
    , "  in  a3"
    ]
