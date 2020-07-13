-- | This module contains methods for optimizing expressions by removing
--   unnecessary nested case expressions.

module HST.Feature.Optimization
  ( optimize
  )
where

import           Polysemy                       ( Member
                                                , Members
                                                , Sem
                                                )

import           HST.CoreAlgorithm              ( getPVarName
                                                , getQNamePat
                                                )
import           HST.Effect.Env                 ( Env
                                                , inEnv
                                                , modifyEnv
                                                )
import           HST.Effect.Fresh               ( Fresh )
import           HST.Environment                ( lookupMatchedPat
                                                , pushMatchedPat
                                                , popMatchedPat
                                                )
import           HST.Environment.Renaming       ( subst
                                                , rename
                                                )
import qualified HST.Frontend.Syntax           as S


-- | Removes all case expressions that are nested inside another case
--   expression for the same variable.
optimize :: (Members '[Env a, Fresh] r, S.EqAST a) => S.Exp a -> Sem r (S.Exp a)
optimize ex = case ex of
  S.InfixApp _ e1 qop e2 -> do
    e1' <- optimize e1
    e2' <- optimize e2
    return $ S.InfixApp S.NoSrcSpan e1' qop e2'
  S.App _ e1 e2 -> do
    e1' <- optimize e1
    e2' <- optimize e2
    return $ S.App S.NoSrcSpan e1' e2'
  S.Lambda _ ps e -> do
    e' <- optimize e
    return $ S.Lambda S.NoSrcSpan ps e'
  S.Let _ b e -> do
    e' <- optimize e
    return $ S.Let S.NoSrcSpan b e'
  S.If _ e1 e2 e3 -> do
    e1' <- optimize e1
    e2' <- optimize e2
    e3' <- optimize e3
    return $ S.If S.NoSrcSpan e1' e2' e3'
  S.Case  _ e   alts -> optimizeCase e alts
  S.Tuple _ bxd es   -> do
    es' <- mapM optimize es
    return $ S.Tuple S.NoSrcSpan bxd es'
  S.List _ es -> do
    es' <- mapM optimize es
    return $ S.List S.NoSrcSpan es'
  S.Paren _ e -> do
    e' <- optimize e
    return $ S.Paren S.NoSrcSpan e'
  c -> return c

-- | Tests whether the given scrutinee of a @case@ expression is a variable
--   that has already been matched by a surrounding @case@ expression.
--
--   If the scrutinee is a variable that has been matched already, the
--   current @case@ expression is redundant and the appropriate alternative
--   can be selected directly.
optimizeCase
  :: (Members '[Env a, Fresh] r, S.EqAST a)
  => S.Exp a
  -> [S.Alt a]
  -> Sem r (S.Exp a)
optimizeCase (S.Var _ varName) alts = do
  mpat <- inEnv $ lookupMatchedPat varName
  case mpat of
    Just pat -> renameAndOpt pat alts
    Nothing  -> addAndOpt varName alts
optimizeCase e alts = do
  e'    <- optimize e
  alts' <- mapM optimizeAlt alts
  return $ S.Case S.NoSrcSpan e' alts'

-- TODO generalise

-- | Gets the right-hand side of the alternative that matches the same
--   constructor as the given pattern, renames variable patterns in the
--   alternative to the names of the corresponding variable patterns of the
--   given pattern and applies 'optimize'.
renameAndOpt
  :: (Members '[Env a, Fresh] r, S.EqAST a)
  => S.Pat a   -- ^ A pattern of a parent @case@ expression on the same scrutinee.
  -> [S.Alt a] -- ^ The alternatives of the current @case@ expression.
  -> Sem r (S.Exp a)
renameAndOpt pat alts =
  let aPaR     = map (\(S.Alt _ p r _) -> (p, r)) alts
      patQ     = getQNamePat pat
      sameCons = filter (\(p, _) -> cheatEq (getQNamePat p) patQ) aPaR
  in  case sameCons of
        []           -> error "Found name in case, but in alts"
        ((p, r) : _) -> do
          let e  = selectExp r
              p1 = selectPats pat
              p2 = selectPats p
          res <- renameAll (zip p2 p1) e
          optimize res

-- | Compares the given 'S.QName's ignoring the distinction between 'S.Ident's
--   and 'S.Symbol's, i.e. @S.Ident "+:"@ amd @S.Symbol "+:"@ are equal.
cheatEq :: S.QName a -> S.QName a -> Bool
cheatEq (S.UnQual _ (S.Symbol _ s1)) (S.UnQual _ (S.Ident  _ s2)) = s1 == s2
cheatEq (S.UnQual _ (S.Ident  _ s1)) (S.UnQual _ (S.Symbol _ s2)) = s1 == s2
cheatEq q1                           q2                           = q1 == q2

-- | Gets the argument patterns of the given constructor pattern.
selectPats :: S.Pat a -> [S.Pat a]
selectPats (S.PApp _ _ pats      ) = pats
selectPats (S.PInfixApp _ p1 _ p2) = [p1, p2]
selectPats _                       = error "selectPat: Unsupported pattern" --not definied for " ++ show p

-- | Gets the actual expression of the given right-hand side without guard.
selectExp :: S.Rhs a -> S.Exp a
selectExp (S.UnGuardedRhs _ e) = e
selectExp _                    = error "selectExp: only unguarded rhs"

-- | Renames the corresponding pairs of variable patterns in the given
--   expression.
renameAll
  :: Member Fresh r => [(S.Pat a, S.Pat a)] -> S.Exp a -> Sem r (S.Exp a)
-- TODO refactor higher order foldr
-- TODO generate one Subst and apply only once
renameAll []               e = return e
renameAll ((from, to) : r) e = do
  f   <- getPVarName from
  t   <- getPVarName to
  res <- renameAll r e
  return $ rename (subst f t) res

-- | Applies 'optimizeAlt' to the given @case@ expression alternatives and
--   constructs a @case@ expression from the optimized alternatives.
--
--   While an alternative is optimized, the state contains a 'matchedPat'
--   entry for the current pair of scrutinee and pattern.
addAndOpt
  :: (Members '[Env a, Fresh] r, S.EqAST a)
  => S.QName a
  -> [S.Alt a]
  -> Sem r (S.Exp a)
addAndOpt v alts = do
  alts' <- mapM bindAndOpt alts
  return $ S.Case S.NoSrcSpan (S.Var S.NoSrcSpan v) alts'
 where
  bindAndOpt a@(S.Alt _ p _ _) = do
    modifyEnv $ pushMatchedPat v p
    alt' <- optimizeAlt a
    modifyEnv $ popMatchedPat v
    return alt'

-- | Optimizes the right-hand side of the given @case@ expression alternative.
optimizeAlt
  :: (Members '[Env a, Fresh] r, S.EqAST a) => S.Alt a -> Sem r (S.Alt a)
optimizeAlt (S.Alt _ p rhs _) = do
  let (S.UnGuardedRhs _ e) = rhs
  e' <- optimize e
  return $ S.Alt S.NoSrcSpan p (S.UnGuardedRhs S.NoSrcSpan e') Nothing
