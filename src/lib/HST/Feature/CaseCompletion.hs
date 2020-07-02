-- | This module contains methods for completing case expressions both in
--   expressions and entire modules.

module HST.Feature.CaseCompletion
  ( completeCase
  , applyCCModule
  )
where

import           HST.CoreAlgorithm              ( match
                                                , err
                                                , Eqs
                                                , isPVar
                                                )
import           HST.Environment.FreshVars      ( PM
                                                , newVars
                                                , newVar
                                                )
import qualified HST.Frontend.Syntax           as S
import qualified HST.Frontend.Build            as B

-- | Takes a given expression and applies the algorithm on it resulting in
--   completed cases
completeCase :: (Eq l, Eq t) => Bool -> S.Exp s l t -> PM s l t (S.Exp s l t)
completeCase insideLet (S.Case _ expr as) = do
  v <- newVar
  let eqs = map getEqFromAlt as   -- [Eqs]
  eqs' <- mapM (\(p, ex) -> completeCase insideLet ex >>= \e -> return (p, e))
               eqs
  res <- match [v] eqs' err
  if not insideLet
    then do
      let (S.Case _ _ resAs) = res
      return $ B.caseE expr resAs  -- test to deconstruct the first case
    else do
      let a = B.alt v res
      return $ B.caseE expr [a]
completeCase il (S.InfixApp _ e1 qop e2) = do
  exp1 <- completeCase il e1
  exp2 <- completeCase il e2
  return $ S.InfixApp B.noSrc exp1 qop exp2
completeCase il (S.App _ e1 e2) = do
  exp1 <- completeCase il e1
  exp2 <- completeCase il e2
  return $ S.App B.noSrc exp1 exp2
completeCase il (S.Lambda _ ps    e) = completeLambda ps e il
completeCase il (S.Let    _ binds e) = do
  e'     <- completeCase il e -- undefined -- TODO
  binds' <- completeBindRhs binds
  return $ S.Let B.noSrc binds' e'
completeCase il (S.If _ e1 e2 e3) = do
  exp1 <- completeCase il e1
  exp2 <- completeCase il e2
  exp3 <- completeCase il e3
  return $ S.If B.noSrc exp1 exp2 exp3
completeCase il (S.Tuple _ boxed es) = do
  exps <- mapM (completeCase il) es  -- complete case für List Expression
  return $ S.Tuple B.noSrc boxed exps
completeCase il (S.Paren _ e1) = do
  exp1 <- completeCase il e1
  return $ S.Paren B.noSrc exp1
completeCase _ v = return v
-- TODO unhandled or not implemented cons missing. Is NegApp used?

completeBindRhs :: (Eq l, Eq t) => S.Binds s l t -> PM s l t (S.Binds s l t)
completeBindRhs (S.BDecls _ dcls) = do
  dcls' <- mapM (applyCCDecl True) dcls
  return $ S.BDecls B.noSrc dcls'
--completeBindRhs _ = error "completeBindRhs: ImplicitBinds not supported yet"

getEqFromAlt :: S.Alt s l t -> Eqs s l t
getEqFromAlt (S.Alt _ pat (S.UnGuardedRhs _ expr) _) = ([pat], expr)
getEqFromAlt _ = error "guarded Rhs in getEqFromAlt"

completeLambda
  :: (Eq l, Eq t)
  => [S.Pat s l]
  -> S.Exp s l t
  -> Bool
  -> PM s l t (S.Exp s l t)
completeLambda ps e insideLet = do
  xs <- newVars (length ps)
  e' <- completeCase insideLet e
  let eq = (ps, e')
  res <- match xs [eq] err
  return $ S.Lambda B.noSrc xs res

applyCCModule :: (Eq l, Eq t) => S.Module s l t -> PM s l t (S.Module s l t)
applyCCModule (S.Module ds) = do
  dcls <- mapM (applyCCDecl False) ds
  return $ S.Module dcls

applyCCDecl :: (Eq l, Eq t) => Bool -> S.Decl s l t -> PM s l t (S.Decl s l t)
applyCCDecl insideLet (S.FunBind _ ms) = do
  nms <- applyCCMatches insideLet ms
  return (S.FunBind B.noSrc nms)
applyCCDecl insideLet (S.PatBind _ p r _) = if isPVar p
  then do
    let e = (\(S.UnGuardedRhs _ x) -> x) r
    e' <- completeCase insideLet e
    return $ S.PatBind B.noSrc p (S.UnGuardedRhs B.noSrc e') B.noBinds
  else error "Toplevel PatBind with no Variable"
applyCCDecl _ v = return v

applyCCMatches
  :: (Eq l, Eq t) => Bool -> [S.Match s l t] -> PM s l t [S.Match s l t]
applyCCMatches insideLet = mapM applyCCMatch
 where
  -- TODO maybe only apply if needed -> isIncomplete?
  applyCCMatch :: (Eq l, Eq t) => S.Match s l t -> PM s l t (S.Match s l t)
  applyCCMatch (S.Match _ n ps rhs _) = case rhs of
    S.UnGuardedRhs _ e -> do
      x <- completeCase insideLet e
      return $ S.Match B.noSrc n ps (S.UnGuardedRhs B.noSrc x) B.noBinds
    S.GuardedRhss _ _ -> error "applyCCMatch: GuardedRhs found"
  applyCCMatch (S.InfixMatch _ p n ps rhs _) = case rhs of
    S.UnGuardedRhs _ e -> do
      x <- completeCase insideLet e
      return $ S.InfixMatch B.noSrc p n ps (S.UnGuardedRhs B.noSrc x) B.noBinds
    S.GuardedRhss _ _ -> error "applyCCMatch: GuardedRhs found"
