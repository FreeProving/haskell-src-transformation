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
  )
where

import           Polysemy                       ( Sem
                                                , makeSem
                                                , reinterpret
                                                )
import           Polysemy.Reader                ( asks
                                                , runReader
                                                )

import           HST.Options                    ( Options(..) )

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
