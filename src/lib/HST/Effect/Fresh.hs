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
  , freshNameWithSrcSpan
  , freshQName
  , freshQNameWithSrcSpan
  , freshVar
  , freshVarWithSrcSpan
  , freshVarPat
  , freshVarPatWithSrcSpan
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
freshName = flip freshNameWithSrcSpan S.NoSrcSpan

-- | Generates a name for a fresh variable and sets the 'S.SrcSpan' to the
--   given value.
freshNameWithSrcSpan
  :: Member Fresh r => String -> S.SrcSpan a -> Sem r (S.Name a)
freshNameWithSrcSpan prefix s = S.Ident s <$> freshIdent prefix

-- | Generates an unqualified name for a fresh variable.
freshQName :: Member Fresh r => String -> Sem r (S.QName a)
freshQName = flip freshQNameWithSrcSpan S.NoSrcSpan

-- | Generates an unqualified name for a fresh variable and sets the
--   'S.SrcSpan' to the given value.
freshQNameWithSrcSpan
  :: Member Fresh r => String -> S.SrcSpan a -> Sem r (S.QName a)
freshQNameWithSrcSpan prefix s = S.UnQual s <$> freshNameWithSrcSpan prefix s

-- | Generates a fresh variable expression.
freshVar :: Member Fresh r => String -> Sem r (S.Exp a)
freshVar = flip freshVarWithSrcSpan S.NoSrcSpan

-- | Generates a fresh variable expression and sets the 'S.SrcSpan' to the
--   given value.
freshVarWithSrcSpan
  :: Member Fresh r => String -> S.SrcSpan a -> Sem r (S.Exp a)
freshVarWithSrcSpan prefix s = S.Var s <$> freshQNameWithSrcSpan prefix s

-- | Generates a fresh variable pattern.
freshVarPat :: Member Fresh r => String -> Sem r (S.Pat a)
freshVarPat = flip freshVarPatWithSrcSpan S.NoSrcSpan

-- | Generates a fresh variable pattern whose 'S.SrcSpan' is identical to the
--   given one.
freshVarPatWithSrcSpan
  :: Member Fresh r => String -> S.SrcSpan a -> Sem r (S.Pat a)
freshVarPatWithSrcSpan prefix s = S.PVar s <$> freshNameWithSrcSpan prefix s

-------------------------------------------------------------------------------
-- Interpretations                                                           --
-------------------------------------------------------------------------------
-- | Interprets a computation that needs fresh variables by generating
--   identifiers of the form @<prefix><N>@ that do not collide with the given
--   set of used identifiers.
runFresh :: Set String -> Sem (Fresh ': r) a -> Sem r a
runFresh usedIdentifiers = evalState Map.empty . freshToState
 where
  -- | Reinterprets 'Fresh' in terms of 'State'.
  freshToState :: Sem (Fresh ': r) a -> Sem (State (Map String Int) ': r) a
  freshToState = reinterpret \case
    FreshIdent prefix -> do
      nextId <- gets $ Map.findWithDefault (0 :: Int) prefix
      let newId = until
            (\n -> (prefix ++ show n) `Set.notMember` usedIdentifiers) (+ 1)
            nextId
      modify $ Map.insert prefix (newId + 1)
      return (prefix ++ show newId)

-------------------------------------------------------------------------------
-- Backward Compatibility                                                    --
-------------------------------------------------------------------------------
-- | The default prefix to use for fresh variables.
--
--   Uses of this prefix should be replaced by more specific fresh variable
--   prefixes.
genericFreshPrefix :: String
genericFreshPrefix = "a"
