-- | This module contains methods for completing case expressions both in
--   expressions and entire modules.
module HST.Feature.CaseCompletion ( applyCCModule ) where

import           Polysemy            ( Member, Members, Sem )

import           HST.CoreAlgorithm   ( Eqs, defaultErrorExp, match )
import           HST.Effect.Env      ( Env )
import           HST.Effect.Fresh
  ( Fresh, freshVarPat, freshVarPatWithSrcSpan, genericFreshPrefix )
import           HST.Effect.GetOpt   ( GetOpt )
import           HST.Effect.Report   ( Report )
import qualified HST.Frontend.Syntax as S
import           HST.Util.Selectors  ( expFromUnguardedRhs )

-- | Takes a given expression and applies the algorithm on it resulting in
--   completed cases
completeCase :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
             => Bool
             -> S.Exp a
             -> Sem r (S.Exp a)
completeCase insideLet (S.Case s expr as) = do
  v <- freshVarPat genericFreshPrefix
  eqs <- mapM getEqFromAlt as
  eqs' <- mapM (\(p, ex) -> completeCase insideLet ex >>= \e -> return (p, e))
    eqs
  res <- match [v] eqs' defaultErrorExp
  if not insideLet
    then do
      let (S.Case _ _ resAs) = res
      return $ S.Case s expr resAs  -- test to deconstruct the first case
    else do
      let a = S.alt v res
      return $ S.Case s expr [a]
completeCase il (S.InfixApp s e1 qop e2)  = do
  e1' <- completeCase il e1
  e2' <- completeCase il e2
  return $ S.InfixApp s e1' qop e2'
completeCase il (S.NegApp s e)            = do
  e' <- completeCase il e
  return $ S.NegApp s e'
completeCase il (S.App s e1 e2)           = do
  e1' <- completeCase il e1
  e2' <- completeCase il e2
  return $ S.App s e1' e2'
completeCase il (S.Lambda s ps e)         = completeLambda s ps e il
completeCase il (S.Let s binds e)         = do
  e' <- completeCase il e -- undefined -- TODO
  binds' <- completeBindRhs binds
  return $ S.Let s binds' e'
completeCase il (S.If s e1 e2 e3)         = do
  e1' <- completeCase il e1
  e2' <- completeCase il e2
  e3' <- completeCase il e3
  return $ S.If s e1' e2' e3'
completeCase il (S.Tuple s boxed es)      = do
  es' <- mapM (completeCase il) es  -- complete case für List Expression
  return $ S.Tuple s boxed es'
completeCase il (S.List s es)             = do
  es' <- mapM (completeCase il) es
  return $ S.List s es'
completeCase il (S.Paren s e)             = do
  e' <- completeCase il e
  return $ S.Paren s e'
completeCase il (S.ExpTypeSig s e t)      = do
  e' <- completeCase il e
  return $ S.ExpTypeSig s e' t
-- Variables, Constructors and literals contain no expressions to complete
-- pattern matching in.
completeCase _ e@(S.Var _ _)              = return e
completeCase _ e@(S.Con _ _)              = return e
completeCase _ e@(S.Lit _ _)              = return e

completeBindRhs :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
                => S.Binds a
                -> Sem r (S.Binds a)
completeBindRhs (S.BDecls s decls) = do
  decls' <- mapM (applyCCDecl True) decls
  return $ S.BDecls s decls'

getEqFromAlt :: Member Report r => S.Alt a -> Sem r (Eqs a)
getEqFromAlt (S.Alt _ pat rhs _) = do
  expr <- expFromUnguardedRhs rhs
  return ([pat], expr)

completeLambda :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
               => S.SrcSpan a
               -> [S.Pat a]
               -> S.Exp a
               -> Bool
               -> Sem r (S.Exp a)
completeLambda s ps e insideLet = do
  let srcSpans = map S.getSrcSpan ps
  xs <- mapM (freshVarPatWithSrcSpan genericFreshPrefix) srcSpans
  e' <- completeCase insideLet e
  let eq = (ps, e')
  res <- match xs [eq] defaultErrorExp
  return $ S.Lambda s xs res

applyCCModule :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
              => S.Module a
              -> Sem r (S.Module a)
applyCCModule (S.Module s origModuleHead moduleName imports decls) = do
  decls' <- mapM (applyCCDecl False) decls
  return $ S.Module s origModuleHead moduleName imports decls'

applyCCDecl :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
            => Bool
            -> S.Decl a
            -> Sem r (S.Decl a)
applyCCDecl insideLet (S.FunBind s ms) = do
  nms <- applyCCMatches insideLet ms
  return (S.FunBind s nms)
applyCCDecl _ v = return v

applyCCMatches :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
               => Bool
               -> [S.Match a]
               -> Sem r [S.Match a]
applyCCMatches insideLet = mapM applyCCMatch
 where
  -- TODO maybe only apply if needed -> isIncomplete?
  applyCCMatch :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
               => S.Match a
               -> Sem r (S.Match a)
  applyCCMatch m = do
    let rhs     = S.matchRhs m
        srcSpan = S.getSrcSpan rhs
    expr <- expFromUnguardedRhs rhs
    expr' <- completeCase insideLet expr
    return $ m { S.matchRhs = S.UnGuardedRhs srcSpan expr' }
