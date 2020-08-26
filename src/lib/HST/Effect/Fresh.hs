{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module defines an effect for computations that can generate fresh
--   variables.
module HST.Effect.Fresh
  ( -- * Effect
    Fresh
    -- * Actions
  , freshIdent
  , freshName
  , freshQName
  , freshVar
  , freshVarPat
    -- * Interpretations
  , runFresh
    -- * Backward Compatibility
  , genericFreshPrefix
  ) where

import           Data.Map.Strict     ( Map )
import qualified Data.Map.Strict     as Map
import           Data.Set            ( Set )
import qualified Data.Set            as Set
import           Polysemy            ( Member, Sem, makeSem, reinterpret )
import           Polysemy.State      ( State, evalState, gets, modify )

import qualified HST.Frontend.Syntax as S

-------------------------------------------------------------------------------
-- Effect and Actions                                                        --
-------------------------------------------------------------------------------
-- | An effect capable of generating names for fresh variables.
data Fresh m a where
  FreshIdent :: String -> Fresh m String

makeSem ''Fresh

-- | Generates a name for a fresh variable.
freshName :: Member Fresh r => String -> Sem r (S.Name a)
freshName prefix = S.Ident S.NoSrcSpan <$> freshIdent prefix

-- | Generates an unqualified name for a fresh variable.
freshQName :: Member Fresh r => String -> Sem r (S.QName a)
freshQName prefix = S.UnQual S.NoSrcSpan <$> freshName prefix

-- | Generates a fresh variable expression.
freshVar :: Member Fresh r => String -> Sem r (S.Exp a)
freshVar prefix = S.Var S.NoSrcSpan <$> freshQName prefix

-- | Generates a fresh variable pattern.
freshVarPat :: Member Fresh r => String -> Sem r (S.Pat a)
freshVarPat prefix = S.PVar S.NoSrcSpan <$> freshName prefix

-------------------------------------------------------------------------------
-- Interpretations                                                           --
-------------------------------------------------------------------------------
-- | Interprets a computation that needs fresh variables by generating
--   identifiers of the form @<prefix><N>@.
runFresh :: Set String -> Sem (Fresh ': r) a -> Sem r a
runFresh usedIdentifiers = evalState Map.empty . freshToState
 where
  -- | Reinterprets 'Fresh' in terms of 'State'.
  freshToState :: Sem (Fresh ': r) a -> Sem (State (Map String Int) ': r) a
  freshToState = reinterpret \case
    FreshIdent prefix -> do
      nextId <- gets $ Map.findWithDefault (0 :: Int) prefix
      let newId = while (\n -> (prefix ++ show n) `Set.member` usedIdentifiers)
            (+ 1) nextId
      modify $ Map.insert prefix (newId + 1)
      return (prefix ++ show newId)

   -- Applies a given function to a given value as long as the condition holds
  while :: (a -> Bool) -> (a -> a) -> a -> a
  while p f x | p x = while p f (f x)
              | otherwise = x

-------------------------------------------------------------------------------
-- Backward Compatibility                                                    --
-------------------------------------------------------------------------------
-- | The default prefix to use for fresh variables.
--
--   Uses of this prefix should be replaced by more specific fresh variable
--   prefixes.
genericFreshPrefix :: String
genericFreshPrefix = "a"
