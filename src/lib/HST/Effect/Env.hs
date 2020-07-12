{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}

-- | This module defines an effect for computations that can read and write
--   the pattern matching compiler's environment.

module HST.Effect.Env
  ( -- * Effect
    Env
    -- * Actions
  , getEnv
  , putEnv
  , inEnv
  , modifyEnv
    -- * Interpretations
  , runEnv
  )
where

import           Polysemy                       ( Member
                                                , Sem
                                                , makeSem
                                                , reinterpret
                                                )
import           Polysemy.State                 ( State
                                                , evalState
                                                , get
                                                , put
                                                )

import           HST.Environment                ( Environment
                                                , emptyEnv
                                                )

-------------------------------------------------------------------------------
-- Effect and Actions                                                        --
-------------------------------------------------------------------------------

-- | An effect capable of reading and writing the pattern matching compiler's
--   environment.
data Env m a where
  GetEnv ::Env m Environment
  PutEnv ::Environment -> Env m ()

makeSem ''Env

-- | Gets a specific component of the current environment by using the
--   supplied projection function.
inEnv :: Member Env r => (Environment -> a) -> Sem r a
inEnv f = f <$> getEnv

-- | Modifies the environment by applying the given transformation on the
--   current environment.
modifyEnv :: Member Env r => (Environment -> Environment) -> Sem r ()
modifyEnv f = do
  env <- getEnv
  putEnv (f env)

-------------------------------------------------------------------------------
-- Interpretations                                                           --
-------------------------------------------------------------------------------

-- | Handles a computation by providing an initially empty environment to
--   read from and write to.
runEnv :: Sem (Env ': r) a -> Sem r a
runEnv = runWithEnv emptyEnv

-- | Handles a computation by providing the supplied environment to read from
--   and write to.
runWithEnv :: Environment -> Sem (Env ': r) a -> Sem r a
runWithEnv initialEnv = evalState initialEnv . envToState
 where
  -- | Reinterprets 'Env' in terms of 'State'.
  envToState :: Sem (Env ': r) a -> Sem (State Environment ': r) a
  envToState = reinterpret \case
    GetEnv      -> get
    PutEnv env' -> put env'
