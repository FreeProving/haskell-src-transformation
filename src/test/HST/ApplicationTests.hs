{-# LANGUAGE RankNTypes #-}

-- | This module contains basic tests for "HST.Application".
module HST.ApplicationTests ( testApplication ) where

import           Polysemy                  ( Members, Sem, runM )
import           Polysemy.Embed            ( Embed )
import           Test.Hspec
  ( Spec, context, describe, it, shouldBe )

import           HST.Application           ( processModule )
import           HST.Effect.Cancel         ( Cancel )
import           HST.Effect.Env            ( runEnv )
import           HST.Effect.Fresh          ( runFresh )
import           HST.Effect.GetOpt         ( GetOpt, runWithArgs )
import           HST.Effect.Report         ( Report, cancelToReport )
import           HST.Effect.SetExpectation
  ( SetExpectation, reportToSetExpectation, setExpectation, setExpectationToIO )
import           HST.Effect.WithFrontend
  ( WithFrontend, parseModule, prettyPrintModule, runWithAllFrontends
  , transformModule, unTransformModule )
import           HST.Frontend.Parser       ( ParsedModule )
import qualified HST.Frontend.Syntax       as S
import           HST.Util.Messages         ( Severity(Info), message )

-------------------------------------------------------------------------------
-- Utility Functions                                                         --
-------------------------------------------------------------------------------
-- | Parses a module for testing purposes.
parseTestModule :: Members '[Cancel, Report, WithFrontend f] r
                => [String]
                -> Sem r (ParsedModule f)
parseTestModule = parseModule "<test-input>" . unlines

-- | Runs the given computation with an empty environment and no additional
--   command line arguments.
runTest
  :: (forall f.
      S.EqAST f
      => Sem '[WithFrontend f, GetOpt, Cancel, Report, SetExpectation, Embed IO]
      ())
  -> IO ()
runTest comp = runM
  $ setExpectationToIO
  $ reportToSetExpectation
  $ cancelToReport (message Info S.NoSrcSpan "The computation was canceled.")
  $ runWithArgs []
  $ runWithAllFrontends comp

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
  inputModule' <- transformModule inputModule
  outputModule <- runEnv . runFresh $ processModule inputModule'
  outputModule' <- unTransformModule outputModule
  expectedOutputModule <- parseTestModule expectedOutput
  outputModule' `prettyModuleShouldBe` expectedOutputModule

-- | Pretty prints both given modules and tests whether the resulting strings
--   are equal modulo whitespace.
prettyModuleShouldBe :: Members '[SetExpectation, WithFrontend f] r
                     => ParsedModule f
                     -> ParsedModule f
                     -> Sem r ()
prettyModuleShouldBe m1 m2 = do
  p1 <- prettyPrintModule m1
  p2 <- prettyPrintModule m2
  setExpectation (p1 `shouldBe` p2)

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
