-- | This module contains the actual implementation of the pattern-matching
--   compilation algorithm.

module Optimization
  ( optimize
  )
where

import           Algo                           ( getPVarName
                                                , getQNamePat
                                                )
import           FreshVars                      ( PM
                                                , matchedPat
                                                , modify
                                                , gets
                                                )
import           Renaming                       ( subst
                                                , rename
                                                )

import qualified Language.Haskell.Exts         as HSE
import qualified Language.Haskell.Exts.Build   as B


-- | Removes all case expressions that are nested inside another case
--   expression for the same variable.
optimize :: HSE.Exp () -> PM (HSE.Exp ())
optimize ex = case ex of
  HSE.InfixApp _ e1 qop e2 -> do
    e1' <- optimize e1
    e2' <- optimize e2
    return $ HSE.InfixApp () e1' qop e2'
  HSE.App _ e1 e2 -> do
    e1' <- optimize e1
    e2' <- optimize e2
    return $ HSE.App () e1' e2'
  HSE.Lambda _ ps e -> do
    e' <- optimize e
    return $ HSE.Lambda () ps e'
  HSE.Let _ b e -> do
    e' <- optimize e
    return $ HSE.Let () b e'
  HSE.If _ e1 e2 e3 -> do
    e1' <- optimize e1
    e2' <- optimize e2
    e3' <- optimize e3
    return $ HSE.If () e1' e2' e3'
  HSE.Case _ e alts  -> optimizeCase e alts
  HSE.Do _ _         -> error "optimize : do is not supported"
  HSE.Tuple _ bxd es -> do
    es' <- mapM optimize es
    return $ HSE.Tuple () bxd es'
  HSE.List _ es -> do
    es' <- mapM optimize es
    return $ HSE.List () es'
  HSE.Paren _ e -> do
    e' <- optimize e
    return $ HSE.Paren () e'
  c -> return c

-- | Tests whether the given scrutinee of a @case@ expression is a variable
--   that has already been matched by a surrounding @case@ expression.
--
--   If the scrutinee is a variable that has been matched already, the
--   current @case@ expression is redundant and the appropriate alternative
--   can be selected directly.
optimizeCase :: HSE.Exp () -> [HSE.Alt ()] -> PM (HSE.Exp ())
optimizeCase e alts
  | isVarExp e = do
    mpats <- gets matchedPat
    case lookup e mpats of                  -- lookupBy ?
      Just pat -> renameAndOpt pat alts  -- look for the correct pattern replace, case exp and rename
      Nothing  -> addAndOpt e alts
  |      -- stackwise add it to first place and then remove first
    otherwise = do
    e'    <- optimize e
    alts' <- optimizeAlts alts
    return $ HSE.Case () e' alts'

-- | Tests whether the given expression is a variable expression.
isVarExp :: HSE.Exp () -> Bool
isVarExp (HSE.Var _ _) = True
isVarExp _             = False

-- TODO generalise

-- | Gets the right-hand side of the alternative that matches the same
--   constructor as the given pattern, renames variable patterns in the
--   alternative to the names of the corresponding variable patterns of the
--   given pattern and applies 'optimize'.
renameAndOpt
  :: HSE.Pat () -- ^ A pattern of a parent @case@ expression on the same scrutinee.
  -> [HSE.Alt ()] -- ^ The alternatives of the current @case@ expression.
  -> PM (HSE.Exp ())
renameAndOpt pat alts =
  let aPaR     = map (\(HSE.Alt _ p r _) -> (p, r)) alts
      patQ     = getQNamePat pat
      sameCons = filter (\(p, _) -> cheatEq (getQNamePat p) patQ) aPaR
  in  case sameCons of
        [] ->
          error
            $  "Found in case but not found in alts : Tried"
            ++ show patQ
            ++ " Searched in "
            ++ show (map fst aPaR)
        ((p, r) : _) -> do
          let e  = selectExp r
              p1 = selectPats pat
              p2 = selectPats p
          res <- renameAll (zip p2 p1) e  -- Fixes the renaming bug -> was p1 p2 before
          optimize res

-- | Compares the given 'HSE.QName's ignoring the distinction between 'HSE.Ident's
--   and 'HSE.Symbol's, i.e. @HSE.Ident "+:"@ amd @HSE.Symbol "+:"@ are equal.
cheatEq :: HSE.QName () -> HSE.QName () -> Bool
cheatEq (HSE.UnQual () (HSE.Symbol () s1)) (HSE.UnQual () (HSE.Ident () s2)) =
  s1 == s2
cheatEq (HSE.UnQual () (HSE.Ident () s1)) (HSE.UnQual () (HSE.Symbol () s2)) =
  s1 == s2
cheatEq q1 q2 = q1 == q2

-- | Gets the argument patterns of the given constructor pattern.
selectPats :: HSE.Pat () -> [HSE.Pat ()]
selectPats (HSE.PApp _ _ pats) = pats
selectPats (HSE.PInfixApp _ p1 _ p2) = [p1, p2]
selectPats p = error $ "selectPat: not definied for " ++ show p

-- | Gets the actual expression of the given right-hand side without guard.
selectExp :: HSE.Rhs () -> HSE.Exp ()
selectExp (HSE.UnGuardedRhs _ e) = e
selectExp _                      = error "selectExp: only unguarded rhs"

-- | Renames the corresponding pairs of variable patterns in the given
--   expression.
renameAll :: [(HSE.Pat (), HSE.Pat ())] -> HSE.Exp () -> PM (HSE.Exp ())
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
addAndOpt :: HSE.Exp () -> [HSE.Alt ()] -> PM (HSE.Exp ())
addAndOpt e alts = do
  alts' <- mapM (bindAndOpt e) alts
  return $ HSE.Case () e alts'
 where
  -- uses the list of Exp Pat as a stack
  bindAndOpt :: HSE.Exp () -> HSE.Alt () -> PM (HSE.Alt ())
  bindAndOpt v a@(HSE.Alt _ p _ _) = do
    stack <- gets matchedPat
    modify $ \state -> state { matchedPat = (v, p) : stack }
    alt' <- optimizeAlt a
    modify $ \state -> state { matchedPat = stack }
    return alt'

-- | Applies 'optimizeAlt' to all given @case@ expression alternatives.
optimizeAlts :: [HSE.Alt ()] -> PM [HSE.Alt ()]
optimizeAlts = mapM optimizeAlt

-- | Optimizes the right-hand side of the given @case@ expression alternative.
optimizeAlt :: HSE.Alt () -> PM (HSE.Alt ())
optimizeAlt (HSE.Alt _ p rhs _) = do
  let (HSE.UnGuardedRhs _ e) = rhs
  e' <- optimize e
  return $ HSE.Alt () p (HSE.UnGuardedRhs () e') B.noBinds
