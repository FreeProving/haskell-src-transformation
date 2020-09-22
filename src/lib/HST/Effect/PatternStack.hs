{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module defines an effect for computations which can save patterns
--   that variables have been matched against on a stack.
module HST.Effect.PatternStack
  ( -- * Effect
    PatternStack(..)
    -- * Actions
  , pushPattern
  , peekPattern
  , popPattern
    -- * Interpretations
  , runPatternStack
  ) where

import           Data.Map.Strict     ( Map )
import qualified Data.Map.Strict     as Map
import           Polysemy            ( Sem, makeSem, reinterpret )
import           Polysemy.State      ( State, evalState, gets, modify )

import qualified HST.Frontend.Syntax as S

-------------------------------------------------------------------------------
-- Effect and Actions                                                        --
-------------------------------------------------------------------------------
-- | An effect capable of pushing and popping patterns that variables have been
--   matched against to a stack.
data PatternStack a m b where
  PushPattern :: S.QName a -> S.Pat a -> PatternStack a m ()
  PeekPattern :: S.QName a -> PatternStack a m (Maybe (S.Pat a))
  PopPattern :: S.QName a -> PatternStack a m ()

makeSem ''PatternStack

-------------------------------------------------------------------------------
-- Interpretations                                                           --
-------------------------------------------------------------------------------
-- | A map of stacks of patterns by variable names.
type StackMap a = Map (S.QName a) [S.Pat a]

-- | Interprets the given computation by providing one stack for each variable.
runPatternStack :: Sem (PatternStack a ': r) b -> Sem r b
runPatternStack = evalState Map.empty . patternStackToState
 where
  patternStackToState
    :: Sem (PatternStack a ': r) b -> Sem (State (StackMap a) ': r) b
  patternStackToState = reinterpret \case
    PushPattern name pat -> modify $ Map.alter (maybeCons pat) name
    PeekPattern name     -> gets $ fmap head . Map.lookup name
    PopPattern name      -> modify $ Map.update maybeTail name

  -- | Like @(:)@ but returns @Just@ a singleton list if the given tail is
  --   @Nothing@.
  maybeCons :: a -> Maybe [a] -> Maybe [a]
  maybeCons x Nothing   = Just [x]
  maybeCons x (Just xs) = Just (x : xs)

  -- | Like @tail@ but returns @Nothing@ if the list is empty.
  maybeTail :: [a] -> Maybe [a]
  maybeTail []       = Nothing
  maybeTail (_ : xs) = Just xs
