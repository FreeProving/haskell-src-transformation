{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}

-- | This module defines an effect for computations that can generate fresh
--   variables.

module HST.Effect.Fresh
  ( -- * Effect
    Fresh
    -- * Actions
  , freshVar
    -- * Interpretations
  , runFresh
  )
where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Polysemy                       ( Sem
                                                , makeSem
                                                , reinterpret
                                                )
import           Polysemy.State                 ( State
                                                , gets
                                                , evalState
                                                , modify
                                                )

-------------------------------------------------------------------------------
-- Effect and Actions                                                        --
-------------------------------------------------------------------------------

-- | An effect capable of generating names for a fresh variable.
data Fresh m a where
  FreshVar ::String -> Fresh m String

makeSem ''Fresh

-------------------------------------------------------------------------------
-- Interpretations                                                           --
-------------------------------------------------------------------------------

-- | Interprets a computation that needs fresh variables by generating
--   identifiers of the form @<prefix><N>@.
runFresh :: Sem (Fresh ': r) a -> Sem r a
runFresh = evalState Map.empty . freshToState
 where
  -- | Reinterprets 'Fresh' in terms of 'State'.
  freshToState :: Sem (Fresh ': r) a -> Sem (State (Map String Int) ': r) a
  freshToState = reinterpret \case
    FreshVar prefix -> do
      nextId <- gets $ Map.findWithDefault (0 :: Int) prefix
      modify $ Map.insert prefix (nextId + 1)
      return (prefix ++ show nextId)
