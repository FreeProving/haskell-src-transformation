{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}

-- | This module defines an effect for computations that depend on the
--   command line options of the pattern matching compiler.

module HST.Effect.GetOpt
  ( -- * Command Line Options
    Options(..)
    -- * Effect
  , GetOpt
    -- * Actions
  , getOpt
    -- * Interpretations
  , runWithOptions
  , runWithArgs
  , runWithArgsIO
  )
where

import           Polysemy                       ( Member
                                                , Members
                                                , Sem
                                                , makeSem
                                                , reinterpret
                                                )
import           Polysemy.Embed                 ( Embed
                                                , embed
                                                )
import           Polysemy.Reader                ( asks
                                                , runReader
                                                )
import           System.Environment             ( getArgs )

import           HST.Effect.Report              ( Report )
import           HST.Options                    ( Options(..)
                                                , parseArgs
                                                )

-------------------------------------------------------------------------------
-- Effect and Actions                                                        --
-------------------------------------------------------------------------------

-- | An effect capable of providing the command line options.
data GetOpt m a where
  GetOpt ::(Options -> a) -> GetOpt m a

makeSem ''GetOpt

-------------------------------------------------------------------------------
-- Interpretations                                                           --
-------------------------------------------------------------------------------

-- | Interprets a computation that needs command line options by providing
--   the given command line options.
runWithOptions :: Options -> Sem (GetOpt ': r) a -> Sem r a
runWithOptions opts = runReader opts . reinterpret \case
  GetOpt selector -> asks selector

-- | Interprets a computation that needs command line options by parsing the
--   given list of command line arguments and providing them to the computation.
--
--   If the command line options cannot be parsed, a fatal error is reported.
runWithArgs :: Member Report r => [String] -> Sem (GetOpt ': r) a -> Sem r a
runWithArgs args comp = do
  opts <- parseArgs args
  runWithOptions opts comp

-- | Interprets a computation that needs command line options by parsing the
--   command line arguments that have been passed to the program and providing
--   them to the computation.
--
--   If the command line options cannot be parsed, a fatal error is reported.
runWithArgsIO :: Members '[Report, Embed IO] r => Sem (GetOpt ': r) a -> Sem r a
runWithArgsIO comp = do
  args <- embed getArgs
  runWithArgs args comp
