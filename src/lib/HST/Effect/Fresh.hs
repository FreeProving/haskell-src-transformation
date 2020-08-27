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
  , freshNameWithSpan
  , freshQName
  , freshQNameWithSpan
  , freshVar
  , freshVarWithSpan
  , freshVarPat
  , freshVarPatWithSpan
    -- * Interpretations
  , runFresh
    -- * Backward Compatibility
  , genericFreshPrefix
  ) where

import           Data.Map.Strict     ( Map )
import qualified Data.Map.Strict     as Map
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

-- | Generates a name for a fresh variable and sets the `SrcSpan` to the given
--   value.
freshNameWithSpan :: Member Fresh r => String -> S.SrcSpan a -> Sem r (S.Name a)
freshNameWithSpan prefix s = S.Ident s <$> freshIdent prefix

-- | Generates an unqualified name for a fresh variable.
freshQName :: Member Fresh r => String -> Sem r (S.QName a)
freshQName prefix = S.UnQual S.NoSrcSpan <$> freshName prefix

-- | Generates an unqualified name for a fresh variable and sets the `SrcSpan`
--   to the given value.
freshQNameWithSpan :: Member Fresh r => String -> S.SrcSpan a -> Sem r (S.QName a)
freshQNameWithSpan prefix s = S.UnQual s <$> freshNameWithSpan prefix s

-- | Generates a fresh variable expression.
freshVar :: Member Fresh r => String -> Sem r (S.Exp a)
freshVar prefix = S.Var S.NoSrcSpan <$> freshQName prefix

-- | Generates a fresh variable expression and sets the `SrcSpan` to the given
--   value.
freshVarWithSpan :: Member Fresh r => String -> S.SrcSpan a -> Sem r (S.Exp a)
freshVarWithSpan prefix s = S.Var s <$> freshQNameWithSpan prefix s

-- | Generates a fresh variable pattern.
freshVarPat :: Member Fresh r => String -> Sem r (S.Pat a)
freshVarPat prefix = S.PVar S.NoSrcSpan <$> freshName prefix

-- | Generates a fresh variable pattern which `SrcSpan` is identical to the
--   given one.
freshVarPatWithSpan :: Member Fresh r => String -> S.SrcSpan a -> Sem r (S.Pat a)
freshVarPatWithSpan prefix s = S.PVar s <$> freshNameWithSpan prefix s

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
    FreshIdent prefix -> do
      nextId <- gets $ Map.findWithDefault (0 :: Int) prefix
      modify $ Map.insert prefix (nextId + 1)
      return (prefix ++ show nextId)

-------------------------------------------------------------------------------
-- Backward Compatibility                                                    --
-------------------------------------------------------------------------------
-- | The default prefix to use for fresh variables.
--
--   Uses of this prefix should be replaced by more specific fresh variable
--   prefixes.
genericFreshPrefix :: String
genericFreshPrefix = "a"
