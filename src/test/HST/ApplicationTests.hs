{-# LANGUAGE RankNTypes #-}

-- | This module contains basic tests for "HST.Application".
module HST.ApplicationTests
  ( testApplication
  )
where

import           Polysemy                       ( Member
                                                , Members
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

import           HST.Application                ( processModule )
import           HST.Effect.Cancel              ( Cancel )
import           HST.Effect.Env                 ( runEnv )
import           HST.Effect.Fresh               ( runFresh )
import           HST.Effect.GetOpt              ( GetOpt
                                                , runWithArgs
                                                )
import           HST.Effect.Report              ( Message(Message)
                                                , Report
                                                , Severity(Info)
                                                , cancelToReport
                                                , runReport
                                                , showPrettyMessage
                                                )
import           HST.Effect.WithFrontend        ( WithFrontend
                                                , parseModule
                                                , prettyPrintModule
                                                , runWithAllFrontends
                                                , transformModule
                                                , unTransformModule
                                                )
import           HST.Frontend.Parser            ( ParsedModule )
import qualified HST.Frontend.Syntax           as S

-------------------------------------------------------------------------------
-- Utility Functions                                                         --
-------------------------------------------------------------------------------

-- | Parses a module for testing purposes.
parseTestModule
  :: Members '[Cancel, Report, WithFrontend f] r
  => [String]
  -> Sem r (ParsedModule f)
parseTestModule = parseModule "<test-input>" . unlines

-- | Applies 'processModule' and transforms the processed module back to the
--   AST of the front end.
processTestModule
  :: (S.EqAST f, Members '[GetOpt, WithFrontend f] r)
  => ParsedModule f
  -> Sem r (ParsedModule f)
processTestModule inputModule = runEnv . runFresh $ do
  inputModule' <- transformModule inputModule
  outputModule <- processModule inputModule'
  unTransformModule inputModule outputModule

-- | Runs the given computation with an empty environment and no additional
--   command line arguments.
runTest
  :: (  forall f
      . S.EqAST f
     => Sem '[WithFrontend f, GetOpt, Cancel, Report, Embed IO] ()
     )
  -> IO ()
runTest comp =
  runM
    $ reportToExpectation
    $ cancelToReport (Message Info "The computation was canceled.")
    $ runWithArgs []
    $ runWithAllFrontends comp

-------------------------------------------------------------------------------
-- Expectation Setters                                                       --
-------------------------------------------------------------------------------

-- | Effect for setting 'Expectation's in computations.
--
--   This is an alias for an embedded @IO@ monad with a more descriptive name.
type SetExpectation = Embed IO

-- | Sets the given expectation.
--
--   This is an alias for @embed@ing an IO action into the computation with
--   a more descriptive name.
setExpectation :: Member SetExpectation r => Expectation -> Sem r ()
setExpectation = embed

-- | Pretty prints both the two given modules and tests whether the resulting
--   strings are equal modulo whitespace.
prettyModuleShouldBe
  :: Members '[WithFrontend f, SetExpectation] r
  => ParsedModule f
  -> ParsedModule f
  -> Sem r ()
prettyModuleShouldBe m1 m2 = do
  p1 <- prettyPrintModule m1
  p2 <- prettyPrintModule m2
  setExpectation (p1 `shouldBe` p2)

-- | Handles the 'Report' effect by asserting that no fatal message is reported.
--
--   If there is a fatal message, all reported messages are included in
--   the error message.
reportToExpectation :: Member (Embed IO) r => Sem (Report ': r) a -> Sem r a
reportToExpectation comp = do
  (ms, mx) <- runReport comp
  case mx of
    Nothing -> embed $ assertFailure $ unlines
      ( ("The following " ++ show (length ms) ++ " messages were reported:")
      : map showPrettyMessage ms
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
  it "should leave functions without pattern matching unchanged"
    $ runTest
    -- $ runWithAllFrontends
    $ do
        input  <- parseTestModule ["module A where", "f :: a -> a", "f x = x"]
        output <- processTestModule input
        output `prettyModuleShouldBe` input
  it "should transform pattern matching into case expressions"
    $ runTest
    -- $ runWithAllFrontends
    $ do
        input <- parseTestModule
          [ "module A where"
          , "lengthL :: [a] -> Int"
          , "lengthL [] = 0"
          , "lengthL(_:xs) = 1 + lengthL xs"
          ]
        output         <- processTestModule input
        expectedOutput <- parseTestModule
          [ "module A where"
          , "lengthL :: [a] -> Int"
          , "lengthL a0 = case a0 of"
          , "  []    -> 0"
          , "  a1:a2 -> 1 + lengthL a2"
          ]
        output `prettyModuleShouldBe` expectedOutput
  it "should transform pattern matching in a partial function"
    $ runTest
    -- $ runWithAllFrontends
    $ do
        input <- parseTestModule
          ["module A where", "head :: [a] -> a", "head (x:xs) = x"]
        output         <- processTestModule input
        expectedOutput <- parseTestModule
          [ "module A where"
          , "head :: [a] -> a"
          , "head a0 = case a0 of"
          , "  a1 : a2 -> a1"
          , "  [] -> undefined"
          ]
        output `prettyModuleShouldBe` expectedOutput
  it "should accept a simple guarded expression"
    $ runTest
    -- $ runWithAllFrontends
    $ do
        input <- parseTestModule
          ["module A where", "id :: a -> a", "id x | otherwise = x"]
        output         <- processTestModule input
        expectedOutput <- parseTestModule
          [ "module A where"
          , "id :: a -> a"
          , "id a0 = let a2 = undefined"
          , "            a1 = case a0 of"
          , "              a3 -> if otherwise then a3"
          , "                                 else a2"
          , "         in a1"
          ]
        output `prettyModuleShouldBe` expectedOutput
  it "should accept a more complex guarded function"
    $ runTest
    -- $ runWithAllFrontends
    $ do
        input <- parseTestModule
          [ "module A where"
          , "useless :: (a -> Bool) -> a -> a -> a"
          , "useless p x y | p x       = x"
          , "              | otherwise = y"
          ]
        output         <- processTestModule input
        expectedOutput <- parseTestModule
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
        output `prettyModuleShouldBe` expectedOutput
