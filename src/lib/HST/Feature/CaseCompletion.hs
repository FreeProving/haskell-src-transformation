{-# LANGUAGE FlexibleContexts #-}

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
                                                )
import           HST.Environment.FreshVars      ( PM
                                                , newVars
                                                , newVar
                                                )
import qualified HST.Frontend.Syntax           as S

-- | Takes a given expression and applies the algorithm on it resulting in
--   completed cases
completeCase :: Eq (S.Exp a) => Bool -> S.Exp a -> PM a (S.Exp a)
completeCase insideLet (S.Case _ expr as) = do
  v <- newVar
  let eqs = map getEqFromAlt as   -- [Eqs]
  eqs' <- mapM (\(p, ex) -> completeCase insideLet ex >>= \e -> return (p, e))
               eqs
  res <- match [v] eqs' err
  if not insideLet
    then do
      let (S.Case _ _ resAs) = res
      return $ S.Case S.NoSrcSpan expr resAs  -- test to deconstruct the first case
    else do
      let a = S.alt v res
      return $ S.Case S.NoSrcSpan expr [a]
completeCase il (S.InfixApp _ e1 qop e2) = do
  exp1 <- completeCase il e1
  exp2 <- completeCase il e2
  return $ S.InfixApp S.NoSrcSpan exp1 qop exp2
completeCase il (S.App _ e1 e2) = do
  exp1 <- completeCase il e1
  exp2 <- completeCase il e2
  return $ S.App S.NoSrcSpan exp1 exp2
completeCase il (S.Lambda _ ps    e) = completeLambda ps e il
completeCase il (S.Let    _ binds e) = do
  e'     <- completeCase il e -- undefined -- TODO
  binds' <- completeBindRhs binds
  return $ S.Let S.NoSrcSpan binds' e'
completeCase il (S.If _ e1 e2 e3) = do
  exp1 <- completeCase il e1
  exp2 <- completeCase il e2
  exp3 <- completeCase il e3
  return $ S.If S.NoSrcSpan exp1 exp2 exp3
completeCase il (S.Tuple _ boxed es) = do
  exps <- mapM (completeCase il) es  -- complete case für List Expression
  return $ S.Tuple S.NoSrcSpan boxed exps
completeCase il (S.Paren _ e1) = do
  exp1 <- completeCase il e1
  return $ S.Paren S.NoSrcSpan exp1
completeCase _ v = return v
-- TODO unhandled or not implemented cons missing. Is NegApp used?

completeBindRhs :: Eq (S.Exp a) => S.Binds a -> PM a (S.Binds a)
completeBindRhs (S.BDecls _ dcls) = do
  dcls' <- mapM (applyCCDecl True) dcls
  return $ S.BDecls S.NoSrcSpan dcls'
--completeBindRhs _ = error "completeBindRhs: ImplicitBinds not supported yet"

getEqFromAlt :: S.Alt a -> Eqs a
getEqFromAlt (S.Alt _ pat (S.UnGuardedRhs _ expr) _) = ([pat], expr)
getEqFromAlt _ = error "guarded Rhs in getEqFromAlt"

completeLambda :: Eq (S.Exp a) => [S.Pat a] -> S.Exp a -> Bool -> PM a (S.Exp a)
completeLambda ps e insideLet = do
  xs <- newVars (length ps)
  e' <- completeCase insideLet e
  let eq = (ps, e')
  res <- match xs [eq] err
  return $ S.Lambda S.NoSrcSpan xs res

applyCCModule :: Eq (S.Exp a) => S.Module a -> PM a (S.Module a)
applyCCModule (S.Module ds) = do
  dcls <- mapM (applyCCDecl False) ds
  return $ S.Module dcls

applyCCDecl :: Eq (S.Exp a) => Bool -> S.Decl a -> PM a (S.Decl a)
applyCCDecl insideLet (S.FunBind _ ms) = do
  nms <- applyCCMatches insideLet ms
  return (S.FunBind S.NoSrcSpan nms)
applyCCDecl _ v = return v

applyCCMatches :: Eq (S.Exp a) => Bool -> [S.Match a] -> PM a [S.Match a]
applyCCMatches insideLet = mapM applyCCMatch
 where
  -- TODO maybe only apply if needed -> isIncomplete?
  applyCCMatch :: Eq (S.Exp a) => S.Match a -> PM a (S.Match a)
  applyCCMatch (S.Match _ n ps rhs _) = case rhs of
    S.UnGuardedRhs _ e -> do
      x <- completeCase insideLet e
      return $ S.Match S.NoSrcSpan n ps (S.UnGuardedRhs S.NoSrcSpan x) Nothing
    S.GuardedRhss _ _ -> error "applyCCMatch: GuardedRhs found"
  applyCCMatch (S.InfixMatch _ p n ps rhs _) = case rhs of
    S.UnGuardedRhs _ e -> do
      x <- completeCase insideLet e
      return $ S.InfixMatch S.NoSrcSpan
                            p
                            n
                            ps
                            (S.UnGuardedRhs S.NoSrcSpan x)
                            Nothing
    S.GuardedRhss _ _ -> error "applyCCMatch: GuardedRhs found"
