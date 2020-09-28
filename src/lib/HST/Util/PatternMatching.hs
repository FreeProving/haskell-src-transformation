-- | This module contains utility functions for matching expressions against
--   patterns.
module HST.Util.PatternMatching ( matchAlt, matchPat ) where

import           Control.Monad       ( guard, zipWithM )
import           Data.Composition    ( (.:) )

import qualified HST.Frontend.Syntax as S
import           HST.Util.Subst      ( Subst, combineSubsts, singleSubst )

-- | Tries to match the pattern of the given @case@-expression alternative
--   with the given expression.
--
--   Returns @Nothing@ if the alternative does not match. Returns the right
--   hand side of the alternative and a substitution for variables that are
--   bound by the pattern if it matches.
matchAlt :: S.Alt a -> S.Exp a -> Maybe (Subst a, S.Rhs a)
matchAlt (S.Alt _ pat rhs _) expr = do
  subst <- matchPat pat expr
  return (subst, rhs)

-- | Matches the given expression against the given pattern.
--
--   Returns @Nothing@ if the pattern does not match. If the pattern matches,
--   the result is a substitution of pattern variables to subterms of the
--   expression.
matchPat :: S.Pat a -> S.Exp a -> Maybe (Subst a)
matchPat = fmap combineSubsts .: matchPat'

-- | Like 'matchPat' but returns a list of single substitutions.
matchPat' :: S.Pat a -> S.Exp a -> Maybe [Subst a]

-- Variable pattern match any expression and bind the variable accordingly.
matchPat' (S.PVar _ name) expr = return [singleSubst name expr]
-- Wildcard patterns match any expression but don't bind a variable.
matchPat' (S.PWildCard _) _ = return []
-- A constructor pattern matches an application of the same constructor with
-- the same arity if all argument patterns match the actual arguments.
matchPat' (S.PApp _ conName pats) expr = do
  (conName', args) <- unConApp expr
  guard (conName == conName' && length pats == length args)
  substs <- zipWithM matchPat' pats args
  return (concat substs)
-- Infix, tuple and list patterns are syntactic sugar.
matchPat' (S.PInfixApp srcSpan p1 conName p2) expr = matchPat'
  (S.PApp srcSpan conName [p1, p2]) expr
matchPat' (S.PTuple srcSpan boxed pats) expr
  = let tupleConName = S.special (S.TupleCon srcSpan boxed (length pats))
    in matchPat' (S.PApp srcSpan tupleConName pats) expr
matchPat' (S.PList srcSpan pats) expr
  = let nilPat                    = S.PApp srcSpan (S.special (S.NilCon srcSpan)) []
        mkConsPat headPat tailPat = S.PApp srcSpan (S.special (S.ConsCon srcSpan))
          [headPat, tailPat]
    in matchPat' (foldr mkConsPat nilPat pats) expr
-- Parentheses around the pattern can be ignored.
matchPat' (S.PParen _ pat) expr = matchPat' pat expr

-- | Decomposes the application of a constructor into the name of the
--   constructor and its arguments.
unConApp :: S.Exp a -> Maybe (S.QName a, [S.Exp a])
unConApp = flip unConApp' []
 where
  unConApp' :: S.Exp a -> [S.Exp a] -> Maybe (S.QName a, [S.Exp a])
  unConApp' (S.Con _ conName) args = return (conName, args)
  -- Arguments of expressions in prefix notation are forwarded to the
  -- applied expression.
  unConApp' (S.App _ expr arg) args = unConApp' expr (arg : args)
  -- Infix applications are just syntactic sugar for prefix applications.
  unConApp' (S.InfixApp _ e1 (S.QConOp _ conName) e2) args
    = return (conName, e1 : e2 : args)
  unConApp' (S.InfixApp _ _ (S.QVarOp _ _) _) _ = Nothing
  -- Tuple and list literals are just syntactic sugar for the application of
  -- tupe or list constructors.
  unConApp' (S.Tuple srcSpan boxed exprs) args = return
    (S.special (S.TupleCon srcSpan boxed (length exprs)), exprs ++ args)
  unConApp' (S.List srcSpan []) args = return (S.special (S.NilCon srcSpan), args)
  unConApp' (S.List srcSpan (expr : exprs)) args
    = return (S.special (S.ConsCon srcSpan), expr : S.List srcSpan exprs : args)
  -- Parentheses around expressions and type signatures can be ignored.
  unConApp' (S.Paren _ expr) args = unConApp' expr args
  unConApp' (S.ExpTypeSig _ expr _) args = unConApp' expr args
  -- All other expressions are not constructor applications.
  unConApp' (S.Var _ _) _ = Nothing
  unConApp' (S.Lit _ _) _ = Nothing
  unConApp' (S.NegApp _ _) _ = Nothing
  unConApp' (S.Lambda _ _ _) _ = Nothing
  unConApp' (S.Let _ _ _) _ = Nothing
  unConApp' (S.If _ _ _ _) _ = Nothing
  unConApp' (S.Case _ _ _) _ = Nothing
