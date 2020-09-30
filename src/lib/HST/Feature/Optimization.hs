-- | This module contains methods for optimizing expressions by removing
--   unnecessary nested case expressions.
module HST.Feature.Optimization ( optimize ) where

import           Data.List.Extra          ( firstJust )
import           Polysemy                 ( Members, Sem )

import           HST.Effect.Fresh         ( Fresh )
import           HST.Effect.Report        ( Report )
import qualified HST.Frontend.Syntax      as S
import           HST.Util.PatternMatching ( matchAlt )
import           HST.Util.Selectors       ( expFromUnguardedRhs )
import           HST.Util.Subst           ( applySubst )

-- | Removes all case expressions that are nested inside another case
--   expression for the same variable.
optimize :: Members '[Fresh, Report] r => S.Exp a -> Sem r (S.Exp a)
optimize (S.InfixApp s e1 qop e2) = do
  e1' <- optimize e1
  e2' <- optimize e2
  return $ S.InfixApp s e1' qop e2'
optimize (S.NegApp s e)           = do
  e' <- optimize e
  return $ S.NegApp s e'
optimize (S.App s e1 e2)          = do
  e1' <- optimize e1
  e2' <- optimize e2
  return $ S.App s e1' e2'
optimize (S.Lambda s ps e)        = do
  e' <- optimize e
  return $ S.Lambda s ps e'
optimize (S.Let s b e)            = do
  e' <- optimize e
  return $ S.Let s b e'
optimize (S.If s e1 e2 e3)        = do
  e1' <- optimize e1
  e2' <- optimize e2
  e3' <- optimize e3
  return $ S.If s e1' e2' e3'
optimize (S.Case s e alts)        = optimizeCase s e alts
optimize (S.Tuple s bxd es)       = do
  es' <- mapM optimize es
  return $ S.Tuple s bxd es'
optimize (S.List s es)            = do
  es' <- mapM optimize es
  return $ S.List s es'
optimize (S.Paren s e)            = do
  e' <- optimize e
  return $ S.Paren s e'
optimize (S.ExpTypeSig s e t)     = do
  e' <- optimize e
  return $ S.ExpTypeSig s e' t
-- Variables, constructors and literals don't contain expressions to optimize.
optimize e@(S.Var _ _)            = return e
optimize e@(S.Con _ _)            = return e
optimize e@(S.Lit _ _)            = return e

-- | Tests whether the given scrutinee of a @case@ expression is a variable
--   that has already been matched by a surrounding @case@ expression.
--
--   If the scrutinee is a variable that has been matched already, the
--   current @case@ expression is redundant and the appropriate alternative
--   can be selected directly.
optimizeCase :: Members '[Fresh, Report] r
             => S.SrcSpan a
             -> S.Exp a
             -> [S.Alt a]
             -> Sem r (S.Exp a)
optimizeCase s e alts = do
  e' <- optimize e
  alts' <- mapM optimizeAlt alts
  case firstJust (`matchAlt` e) alts' of
    Nothing           -> return $ S.Case s e' alts'
    Just (subst, rhs) -> do
      expr <- expFromUnguardedRhs rhs
      return $ applySubst subst expr

-- | Optimizes the right-hand side of the given @case@ expression alternative.
optimizeAlt :: Members '[Fresh, Report] r => S.Alt a -> Sem r (S.Alt a)
optimizeAlt (S.Alt s p rhs _) = do
  e <- expFromUnguardedRhs rhs
  e' <- optimize e
  return $ S.Alt s p (S.UnGuardedRhs (S.getSrcSpan e') e') Nothing
