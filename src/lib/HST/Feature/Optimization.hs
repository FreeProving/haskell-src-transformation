-- | This module contains methods for optimizing expressions by removing
--   unnecessary nested case expressions.

module HST.Feature.Optimization
  ( optimize
  )
where

import           Control.Monad.Extra            ( findM )
import           Polysemy                       ( Member
                                                , Members
                                                , Sem
                                                )

import           HST.Effect.Fresh               ( Fresh )
import           HST.Effect.Report              ( Message(..)
                                                , Report
                                                , Severity(Error)
                                                , reportFatal
                                                )
import           HST.Effect.PatternStack        ( PatternStack
                                                , pushPattern
                                                , peekPattern
                                                , popPattern
                                                , runPatternStack
                                                )
import           HST.Environment.Renaming       ( subst
                                                , rename
                                                )
import qualified HST.Frontend.Syntax           as S
import           HST.Util.Selectors             ( expFromUnguardedRhs
                                                , getAltConName
                                                , getPatConName
                                                , getPatVarName
                                                )

-- | Removes all case expressions that are nested inside another case
--   expression for the same variable.
optimize :: Members '[Fresh, Report] r => S.Exp a -> Sem r (S.Exp a)
optimize = runPatternStack . optimize'

-- | Like 'optimize' but can access a stack of patterns for each local
--   variable to remember which patterns variables have been matched
--   against.
optimize'
  :: Members '[PatternStack a, Fresh, Report] r => S.Exp a -> Sem r (S.Exp a)
optimize' (S.InfixApp _ e1 qop e2) = do
  e1' <- optimize' e1
  e2' <- optimize' e2
  return $ S.InfixApp S.NoSrcSpan e1' qop e2'
optimize' (S.NegApp _ e) = do
  e' <- optimize' e
  return $ S.NegApp S.NoSrcSpan e'
optimize' (S.App _ e1 e2) = do
  e1' <- optimize' e1
  e2' <- optimize' e2
  return $ S.App S.NoSrcSpan e1' e2'
optimize' (S.Lambda _ ps e) = do
  e' <- optimize' e
  return $ S.Lambda S.NoSrcSpan ps e'
optimize' (S.Let _ b e) = do
  e' <- optimize' e
  return $ S.Let S.NoSrcSpan b e'
optimize' (S.If _ e1 e2 e3) = do
  e1' <- optimize' e1
  e2' <- optimize' e2
  e3' <- optimize' e3
  return $ S.If S.NoSrcSpan e1' e2' e3'
optimize' (S.Case  _ e   alts) = optimizeCase e alts
optimize' (S.Tuple _ bxd es  ) = do
  es' <- mapM optimize' es
  return $ S.Tuple S.NoSrcSpan bxd es'
optimize' (S.List _ es) = do
  es' <- mapM optimize' es
  return $ S.List S.NoSrcSpan es'
optimize' (S.Paren _ e) = do
  e' <- optimize' e
  return $ S.Paren S.NoSrcSpan e'
optimize' (S.ExpTypeSig _ e t) = do
  e' <- optimize' e
  return $ S.ExpTypeSig S.NoSrcSpan e' t

-- Variables, constructors and literals don't contain expressions to optimize'.
optimize' e@(S.Var _ _) = return e
optimize' e@(S.Con _ _) = return e
optimize' e@(S.Lit _ _) = return e

-- | Tests whether the given scrutinee of a @case@ expression is a variable
--   that has already been matched by a surrounding @case@ expression.
--
--   If the scrutinee is a variable that has been matched already, the
--   current @case@ expression is redundant and the appropriate alternative
--   can be selected directly.
optimizeCase
  :: Members '[PatternStack a, Fresh, Report] r
  => S.Exp a
  -> [S.Alt a]
  -> Sem r (S.Exp a)
optimizeCase (S.Var _ varName) alts = do
  mpat <- peekPattern varName
  case mpat of
    Just pat -> renameAndOpt pat alts
    Nothing  -> addAndOpt varName alts
optimizeCase e alts = do
  e'    <- optimize' e
  alts' <- mapM optimizeAlt alts
  return $ S.Case S.NoSrcSpan e' alts'

-- TODO generalise

-- | Gets the right-hand side of the alternative that matches the same
--   constructor as the given pattern, renames variable patterns in the
--   alternative to the names of the corresponding variable patterns of the
--   given pattern and applies 'optimize''.
renameAndOpt
  :: Members '[PatternStack a, Fresh, Report] r
  => S.Pat a   -- ^ Pattern of a parent @case@ expression on the same scrutinee.
  -> [S.Alt a] -- ^ The alternatives of the current @case@ expression.
  -> Sem r (S.Exp a)
renameAndOpt pat alts = do
  matchingAlt <- findM (`altMatchesPat` pat) alts
  case matchingAlt of
    Nothing -> reportFatal $ Message Error $ "Found no possible alternative."
    Just (S.Alt _ pat' rhs _) -> do
      expr  <- expFromUnguardedRhs rhs
      pats  <- selectPats pat
      pats' <- selectPats pat'
      expr' <- renameAll (zip pats' pats) expr
      optimize' expr'

-- | Tests whether the given alternative matches the given pattern.
altMatchesPat :: Member Report r => S.Alt a -> S.Pat a -> Sem r Bool
altMatchesPat alt pat = do
  -- TODO we actually need to unify the patterns here.
  patConName <- getPatConName pat
  altConName <- getAltConName alt
  return (patConName `cheatEq` altConName)

-- | Compares the given 'S.QName's ignoring the distinction between 'S.Ident's
--   and 'S.Symbol's, i.e. @S.Ident "+:"@ amd @S.Symbol "+:"@ are equal.
cheatEq :: S.QName a -> S.QName a -> Bool
cheatEq (S.UnQual _ (S.Symbol _ s1)) (S.UnQual _ (S.Ident  _ s2)) = s1 == s2
cheatEq (S.UnQual _ (S.Ident  _ s1)) (S.UnQual _ (S.Symbol _ s2)) = s1 == s2
cheatEq q1                           q2                           = q1 == q2

-- | Gets the argument patterns of the given constructor pattern.
selectPats :: Member Report r => S.Pat a -> Sem r [S.Pat a]
selectPats (S.PApp _ _ pats      ) = return pats
selectPats (S.PInfixApp _ p1 _ p2) = return [p1, p2]
selectPats _ =
  reportFatal $ Message Error $ "Expected prefix or infix constructor pattern."

-- | Renames the corresponding pairs of variable patterns in the given
--   expression.
renameAll
  :: Members '[Fresh, Report] r
  => [(S.Pat a, S.Pat a)]
  -> S.Exp a
  -> Sem r (S.Exp a)
-- TODO refactor higher order foldr
-- TODO generate one Subst and apply only once
renameAll []               e = return e
renameAll ((from, to) : r) e = do
  f   <- getPatVarName from
  t   <- getPatVarName to
  res <- renameAll r e
  return $ rename (subst f t) res

-- | Applies 'optimizeAlt' to the given @case@ expression alternatives and
--   constructs a @case@ expression from the optimized alternatives.
--
--   While an alternative is optimized, the pattern is pushed to the stack
--   of matched patterns for the scrutinee in the environment.
addAndOpt
  :: Members '[PatternStack a, Fresh, Report] r
  => S.QName a
  -> [S.Alt a]
  -> Sem r (S.Exp a)
addAndOpt v alts = do
  alts' <- mapM bindAndOpt alts
  return $ S.Case S.NoSrcSpan (S.Var S.NoSrcSpan v) alts'
 where
  bindAndOpt a@(S.Alt _ p _ _) = do
    pushPattern v p
    alt' <- optimizeAlt a
    popPattern v
    return alt'

-- | Optimizes the right-hand side of the given @case@ expression alternative.
optimizeAlt
  :: Members '[PatternStack a, Fresh, Report] r => S.Alt a -> Sem r (S.Alt a)
optimizeAlt (S.Alt _ p rhs _) = do
  e  <- expFromUnguardedRhs rhs
  e' <- optimize' e
  return $ S.Alt S.NoSrcSpan p (S.UnGuardedRhs S.NoSrcSpan e') Nothing
