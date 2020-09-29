{-# LANGUAGE RankNTypes #-}

-- | This module contains basic tests for "HST.Application".
module HST.ApplicationTests ( testApplication ) where

import           Polysemy                  ( Members, Sem )
import           Test.Hspec                ( Spec, context, describe, it )

import           HST.Application
  ( createModuleInterface, initializeEnvironment, processModule )
import           HST.Effect.Cancel         ( Cancel )
import           HST.Effect.Env            ( runWithEnv )
import           HST.Effect.Fresh          ( runFresh )
import           HST.Effect.GetOpt         ( GetOpt )
import           HST.Effect.InputModule    ( runInputModule )
import           HST.Effect.Report         ( Report )
import           HST.Effect.SetExpectation ( SetExpectation )
import           HST.Effect.WithFrontend   ( WithFrontend )
import qualified HST.Frontend.Syntax       as S
import           HST.Test.Expectation      ( prettyModuleShouldBe )
import           HST.Test.Parser           ( parseTestModule )
import           HST.Test.Runner           ( runTest )
import           HST.Util.Selectors        ( findIdentifiers )

-------------------------------------------------------------------------------
-- Expectation Setters                                                       --
-------------------------------------------------------------------------------
-- | Parses the given modules, initializes the environment for the the input
--   module, processes it with 'processModule' and sets the expectation that
--   the given output module is produced.
shouldTransformTo
  :: ( S.EqAST f
     , Members '[GetOpt, Cancel, Report, SetExpectation, WithFrontend f] r
     )
  => [String]
  -> [String]
  -> Sem r ()
shouldTransformTo input expectedOutput = do
  inputModule <- parseTestModule input
  let testFileName = "<test-input>"
  env <- runInputModule
    [(testFileName, (inputModule, createModuleInterface inputModule))]
    $ initializeEnvironment testFileName
  outputModule <- runWithEnv env . runFresh (findIdentifiers inputModule)
    $ processModule inputModule
  expectedOutputModule <- parseTestModule expectedOutput
  outputModule `prettyModuleShouldBe` expectedOutputModule

  -- | Parses the given modules, initializes the environment for the the input
  --   module at the head of the given list, processes it with 'processModule'
  --   and sets the expectation that the given output module is produced.
shouldTransformModulesTo
  :: ( S.EqAST f
     , Members '[GetOpt, Cancel, Report, SetExpectation, WithFrontend f] r
     )
  => [[String]]
  -> [String]
  -> Sem r ()
shouldTransformModulesTo inputs expectedOutput = do
  inputModules <- mapM parseTestModule inputs
  let n               = length inputs
      fileNames       = zipWith (\s n' -> "<" ++ s ++ show n' ++ ">")
        (replicate n "test-input") [1 .. n]
      inputModuleList = zip fileNames
        (map (\m -> (m, createModuleInterface m)) inputModules)
  env <- runInputModule inputModuleList (initializeEnvironment (head fileNames))
  outputModule <- runWithEnv env
    . runFresh (findIdentifiers (head inputModules))
    $ processModule (head inputModules)
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
  it "should transform a module which uses a data type from a different module"
    $ runTest
    $ let ms = [ [ "module B where"
                   , "import A"
                   , "mapTree f (Leaf x) = Leaf (f x)"
                   , "mapTree f (Branch l r) = Branch (mapTree f l) (mapTree f r)"
                   ]
               , [ "module A where"
                   , "data Tree a = Leaf a | Branch (Tree a) (Tree a)"
                   ]
               ]
          m  = [ "module B where"
               , "import A"
               , "mapTree a0 a1 = case a1 of"
               , "  Leaf a2      -> Leaf (a0 a2)"
               , "  Branch a4 a5 -> Branch (mapTree a0 a4) (mapTree a0 a5)"
               ]
      in shouldTransformModulesTo ms m
  it "should not change the name of a function defined in another module"
    $ runTest
    $ let ms = [ [ "module B where"
                   , "import A"
                   , "incList [] = []"
                   , "incList (n:ns) = inc n : incList ns"
                   ]
               , ["module A where", "inc n = n + 1"]
               ]
          m  = [ "module B where"
               , "import A"
               , "incList a0 = case a0 of"
               , "  [] -> []"
               , "  a1:a2 -> inc a1 : incList a2"
               ]
      in shouldTransformModulesTo ms m
  context "substitution in modules" $ do
    it "avoids capture of variables shadowed in let expressions"
      $ runTest
      $ let m        = ["module A where", "f (x, y) = let x = y in x"]
            expected = [ "module A where"
                       , "f a0 = case a0 of"
                       , "  (a1, a2) -> let x = a2 in x"
                       ]
        in m `shouldTransformTo` expected
    {- TODO The following test should be used instead of the replacement that
        is not commented out, after the corresponding issue has been fixed.
    it "avoids capture of variables shadowed in case expressions" $ runTest $
      let m        = [ "module A where"
                     , "f (x, y) = case x of"
                     , "             y -> (x, y)"]
          expected = [ "module A where"
                     , "f a0 = case a0 of"
                     , "  (a1, a2) -> case a1 of"
                     , "    y -> (a1, y)"]
      in m `shouldTransformTo` expected -}
    it "avoids capture of variables shadowed in case expressions"
      $ runTest
      $ let m        = [ "module A where"
                       , "f (x, y) ="
                       , "  let r = case x of"
                       , "            y -> (x, y)"
                       , "  in  r"
                       ]
            expected = [ "module A where"
                       , "f a1 = case a1 of"
                       , "  (a2, a3) ->"
                       , "    let r = case a2 of"
                       , "              a0 -> (a2, a0)"
                       , "    in  r"
                       ]
        in m `shouldTransformTo` expected
