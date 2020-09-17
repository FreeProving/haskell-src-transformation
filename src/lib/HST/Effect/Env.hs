{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module defines an effect for computations that can read the pattern
--   matching compiler's environment.
module HST.Effect.Env
  ( -- * Effect
    Env
    -- * Actions
  , getEnv
  , inEnv
    -- * Interpretations
  , runWithEnv
  ) where

import           Polysemy        ( Member, Sem, makeSem, interpret )

import           HST.Environment ( Environment )

-------------------------------------------------------------------------------
-- Effect and Actions                                                        --
-------------------------------------------------------------------------------
-- | An effect capable of reading the pattern matching compiler's environment.
data Env a m b where
  GetEnv :: Env a m (Environment a)

makeSem ''Env

-- | Gets a specific component of the current environment by using the
--   supplied projection function.
inEnv :: Member (Env a) r => (Environment a -> b) -> Sem r b
inEnv f = f <$> getEnv

-------------------------------------------------------------------------------
-- Interpretations                                                           --
-------------------------------------------------------------------------------
-- | Handles a computation by providing the supplied environment to read from.
runWithEnv :: Environment a -> Sem (Env a ': r) b -> Sem r b
runWithEnv env = interpret \case
    GetEnv -> return env
