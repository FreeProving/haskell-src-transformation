-- | This module contains methods for optimizing expressions by removing
--   unnecessary nested case expressions.

module HST.Feature.Optimization
  ( optimize
  )
where

import           HST.CoreAlgorithm              ( getPVarName
                                                , getQNamePat
                                                )
import           HST.Environment.FreshVars      ( PM
                                                , matchedPat
                                                , modify
                                                , gets
                                                )
import           HST.Environment.Renaming       ( subst
                                                , rename
                                                )
import qualified HST.Frontend.Syntax           as S
import qualified HST.Frontend.Build            as B


-- | Removes all case expressions that are nested inside another case
--   expression for the same variable.
optimize :: (Eq l, Eq t) => S.Exp s l t -> PM s l t (S.Exp s l t)
optimize ex = case ex of
  S.InfixApp _ e1 qop e2 -> do
    e1' <- optimize e1
    e2' <- optimize e2
    return $ S.InfixApp B.noSrc e1' qop e2'
  S.App _ e1 e2 -> do
    e1' <- optimize e1
    e2' <- optimize e2
    return $ S.App B.noSrc e1' e2'
  S.Lambda _ ps e -> do
    e' <- optimize e
    return $ S.Lambda B.noSrc ps e'
  S.Let _ b e -> do
    e' <- optimize e
    return $ S.Let B.noSrc b e'
  S.If _ e1 e2 e3 -> do
    e1' <- optimize e1
    e2' <- optimize e2
    e3' <- optimize e3
    return $ S.If B.noSrc e1' e2' e3'
  S.Case _ e alts  -> optimizeCase e alts
  S.Tuple _ bxd es -> do
    es' <- mapM optimize es
    return $ S.Tuple B.noSrc bxd es'
  S.List _ es -> do
    es' <- mapM optimize es
    return $ S.List B.noSrc es'
  S.Paren _ e -> do
    e' <- optimize e
    return $ S.Paren B.noSrc e'
  c -> return c

-- | Tests whether the given scrutinee of a @case@ expression is a variable
--   that has already been matched by a surrounding @case@ expression.
--
--   If the scrutinee is a variable that has been matched already, the
--   current @case@ expression is redundant and the appropriate alternative
--   can be selected directly.
optimizeCase
  :: (Eq l, Eq t) => S.Exp s l t -> [S.Alt s l t] -> PM s l t (S.Exp s l t)
optimizeCase e alts
  | isVarExp e = do
    mpats <- gets matchedPat
    case lookup e mpats of               -- lookupBy ?
      Just pat -> renameAndOpt pat alts  -- look for the correct pattern replace, case exp and rename
      Nothing  -> addAndOpt e alts
  |      -- stackwise add it to first place and then remove first
    otherwise = do
    e'    <- optimize e
    alts' <- optimizeAlts alts
    return $ S.Case B.noSrc e' alts'

-- | Tests whether the given expression is a variable expression.
isVarExp :: S.Exp s l t -> Bool
isVarExp (S.Var _ _) = True
isVarExp _           = False

-- TODO generalise

-- | Gets the right-hand side of the alternative that matches the same
--   constructor as the given pattern, renames variable patterns in the
--   alternative to the names of the corresponding variable patterns of the
--   given pattern and applies 'optimize'.
renameAndOpt
  :: (Eq l, Eq t)
  => S.Pat s l     -- ^ A pattern of a parent @case@ expression on the same scrutinee.
  -> [S.Alt s l t] -- ^ The alternatives of the current @case@ expression.
  -> PM s l t (S.Exp s l t)
renameAndOpt pat alts =
  let aPaR     = map (\(S.Alt _ p r _) -> (p, r)) alts
      patQ     = getQNamePat pat
      sameCons = filter (\(p, _) -> cheatEq (getQNamePat p) patQ) aPaR
  in  case sameCons of
        []           -> error "Found name in case, but in alts"
            {-$  "Found in case but not found in alts : Tried"
            ++ show patQ
            ++ " Searched in "
            ++ show (map fst aPaR)-}
        ((p, r) : _) -> do
          let e  = selectExp r
              p1 = selectPats pat
              p2 = selectPats p
          res <- renameAll (zip p2 p1) e  -- Fixes the renaming bug -> was p1 p2 before
          optimize res

-- | Compares the given 'S.QName's ignoring the distinction between 'S.Ident's
--   and 'S.Symbol's, i.e. @S.Ident "+:"@ amd @S.Symbol "+:"@ are equal.
cheatEq :: S.QName s -> S.QName s -> Bool
cheatEq (S.UnQual _ (S.Symbol _ s1)) (S.UnQual _ (S.Ident  _ s2)) = s1 == s2
cheatEq (S.UnQual _ (S.Ident  _ s1)) (S.UnQual _ (S.Symbol _ s2)) = s1 == s2
cheatEq q1                           q2                           = q1 == q2

-- | Gets the argument patterns of the given constructor pattern.
selectPats :: S.Pat s l -> [S.Pat s l]
selectPats (S.PApp _ _ pats      ) = pats
selectPats (S.PInfixApp _ p1 _ p2) = [p1, p2]
selectPats _                       = error $ "selectPat: Unsupported pattern" --not definied for " ++ show p

-- | Gets the actual expression of the given right-hand side without guard.
selectExp :: S.Rhs s l t -> S.Exp s l t
selectExp (S.UnGuardedRhs _ e) = e
selectExp _                    = error "selectExp: only unguarded rhs"

-- | Renames the corresponding pairs of variable patterns in the given
--   expression.
renameAll :: [(S.Pat s l, S.Pat s l)] -> S.Exp s l t -> PM s l t (S.Exp s l t)
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
  :: (Eq l, Eq t) => S.Exp s l t -> [S.Alt s l t] -> PM s l t (S.Exp s l t)
addAndOpt e alts = do
  alts' <- mapM (bindAndOpt e) alts
  return $ S.Case B.noSrc e alts'
 where
  -- uses the list of Exp Pat as a stack
  bindAndOpt
    :: (Eq l, Eq t) => S.Exp s l t -> S.Alt s l t -> PM s l t (S.Alt s l t)
  bindAndOpt v a@(S.Alt _ p _ _) = do
    stack <- gets matchedPat
    modify $ \state -> state { matchedPat = (v, p) : stack }
    alt' <- optimizeAlt a
    modify $ \state -> state { matchedPat = stack }
    return alt'

-- | Applies 'optimizeAlt' to all given @case@ expression alternatives.
optimizeAlts :: (Eq l, Eq t) => [S.Alt s l t] -> PM s l t [S.Alt s l t]
optimizeAlts = mapM optimizeAlt

-- | Optimizes the right-hand side of the given @case@ expression alternative.
optimizeAlt :: (Eq l, Eq t) => S.Alt s l t -> PM s l t (S.Alt s l t)
optimizeAlt (S.Alt _ p rhs _) = do
  let (S.UnGuardedRhs _ e) = rhs
  e' <- optimize e
  return $ S.Alt B.noSrc p (S.UnGuardedRhs B.noSrc e') B.noBinds
