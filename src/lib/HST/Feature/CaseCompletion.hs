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

import qualified Language.Haskell.Exts.Build   as B
import qualified Language.Haskell.Exts.Syntax  as HSE

-- | Takes a given expression and applies the algorithm on it resulting in
--   completed cases
completeCase :: Bool -> HSE.Exp () -> PM (HSE.Exp ())
completeCase insideLet (HSE.Case _ expr as) = do
  v <- newVar
  let eqs = map getEqFromAlt as   -- [Eqs]
  eqs' <- mapM (\(p, ex) -> completeCase insideLet ex >>= \e -> return (p, e))
               eqs
  res <- match [v] eqs' err
  if not insideLet
    then do
      let (HSE.Case _ _ resAs) = res
      return $ B.caseE expr resAs  -- test to deconstruct the first case
    else do
      let a = B.alt v res
      return $ B.caseE expr [a]
completeCase il (HSE.InfixApp _ e1 qop e2) = do
  exp1 <- completeCase il e1
  exp2 <- completeCase il e2
  return $ HSE.InfixApp () exp1 qop exp2
completeCase il (HSE.App _ e1 e2) = do
  exp1 <- completeCase il e1
  exp2 <- completeCase il e2
  return $ HSE.App () exp1 exp2
completeCase il (HSE.Lambda _ ps    e) = completeLambda ps e il
completeCase il (HSE.Let    _ binds e) = do
  e'     <- completeCase il e -- undefined -- TODO
  binds' <- completeBindRhs binds
  return $ HSE.Let () binds' e'
completeCase il (HSE.If _ e1 e2 e3) = do
  exp1 <- completeCase il e1
  exp2 <- completeCase il e2
  exp3 <- completeCase il e3
  return $ HSE.If () exp1 exp2 exp3
completeCase il (HSE.Tuple _ boxed es) = do
  exps <- mapM (completeCase il) es  -- complete case für List Expression
  return $ HSE.Tuple () boxed exps
completeCase il (HSE.Paren _ e1) = do
  exp1 <- completeCase il e1
  return $ HSE.Paren () exp1
completeCase _ v = return v
-- TODO unhandled or not implemented cons missing. Is NegApp used?



completeBindRhs :: HSE.Binds () -> PM (HSE.Binds ())
completeBindRhs (HSE.BDecls _ dcls) = do
  dcls' <- mapM (applyCCDecl True) dcls
  return $ HSE.BDecls () dcls'
completeBindRhs _ = error "completeBindRhs: ImplicitBinds not supported yet"


getEqFromAlt :: HSE.Alt () -> Eqs
getEqFromAlt (HSE.Alt _ pat (HSE.UnGuardedRhs _ expr) _) = ([pat], expr)
getEqFromAlt _ = error "guarded Rhs in getEqFromAlt"

completeLambda :: [HSE.Pat ()] -> HSE.Exp () -> Bool -> PM (HSE.Exp ())
completeLambda ps e insideLet = do
  xs <- newVars (length ps)
  e' <- completeCase insideLet e
  let eq = (ps, e')
  res <- match xs [eq] err
  return $ HSE.Lambda () xs res


applyCCModule :: HSE.Module () -> PM (HSE.Module ())
applyCCModule (HSE.Module _ mmh mps ids ds) = do
  dcls <- mapM (applyCCDecl False) ds
  return $ HSE.Module () mmh mps ids dcls
applyCCModule _ = error "applyCCModule: not on module"

applyCCDecl :: Bool -> HSE.Decl () -> PM (HSE.Decl ())
applyCCDecl insideLet (HSE.FunBind _ ms) = do
  nms <- applyCCMatches insideLet ms
  return (HSE.FunBind () nms)
applyCCDecl insideLet (HSE.PatBind _ p r _) = if isPVar p
  then do
    let e = (\(HSE.UnGuardedRhs _ x) -> x) r
    e' <- completeCase insideLet e
    return $ HSE.PatBind () p (HSE.UnGuardedRhs () e') B.noBinds
  else error "Toplevel PatBind with no Variable"
applyCCDecl _ v = return v

applyCCMatches :: Bool -> [HSE.Match ()] -> PM [HSE.Match ()]
applyCCMatches insideLet = mapM applyCCMatch
 where
  -- TODO maybe only apply if needed -> isIncomplete?
  applyCCMatch :: HSE.Match () -> PM (HSE.Match ())
  applyCCMatch (HSE.Match _ n ps rhs _) = case rhs of
    HSE.UnGuardedRhs _ e -> do
      x <- completeCase insideLet e
      return $ HSE.Match () n ps (HSE.UnGuardedRhs () x) B.noBinds
    HSE.GuardedRhss _ _ -> error "applyCCMatch: GuardedRhs found"
  applyCCMatch (HSE.InfixMatch _ p n ps rhs _) = case rhs of
    HSE.UnGuardedRhs _ e -> do
      x <- completeCase insideLet e
      return $ HSE.InfixMatch () p n ps (HSE.UnGuardedRhs () x) B.noBinds
    HSE.GuardedRhss _ _ -> error "applyCCMatch: GuardedRhs found"
