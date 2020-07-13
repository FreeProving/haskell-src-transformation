{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}

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
  )
where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Polysemy                       ( Member
                                                , Sem
                                                , makeSem
                                                , reinterpret
                                                )
import           Polysemy.State                 ( State
                                                , gets
                                                , evalState
                                                , modify
                                                )

import qualified HST.Frontend.Syntax           as S

-------------------------------------------------------------------------------
-- Effect and Actions                                                        --
-------------------------------------------------------------------------------

-- | An effect capable of generating names for a fresh variable.
data Fresh m a where
  FreshIdent ::String -> Fresh m String

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
