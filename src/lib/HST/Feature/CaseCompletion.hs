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
import           HST.Util.Selectors  ( expFromUnguardedRhs, getPatVarName )
import           HST.Util.Subst      ( applySubst, singleSubst )

-- | Takes a given expression and applies the algorithm on it resulting in
--   completed cases
completeCase :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
             => S.Exp a
             -> Sem r (S.Exp a)
completeCase (S.Case _ expr as)       = do
  v <- freshVarPat genericFreshPrefix
  expr' <- completeCase expr
  eqs <- mapM getEqFromAlt as
  eqs' <- mapM (\(p, ex) -> completeCase ex >>= \e -> return (p, e)) eqs
  res <- match [v] eqs' defaultErrorExp
  v' <- getPatVarName v
  return $ applySubst (singleSubst (S.toQName v') expr') res
completeCase (S.InfixApp s e1 qop e2) = do
  e1' <- completeCase e1
  e2' <- completeCase e2
  return $ S.InfixApp s e1' qop e2'
completeCase (S.NegApp s e)           = do
  e' <- completeCase e
  return $ S.NegApp s e'
completeCase (S.App s e1 e2)          = do
  e1' <- completeCase e1
  e2' <- completeCase e2
  return $ S.App s e1' e2'
completeCase (S.Lambda s ps e)        = completeLambda s ps e
completeCase (S.Let s binds e)        = do
  e' <- completeCase e -- undefined -- TODO
  binds' <- completeBindRhs binds
  return $ S.Let s binds' e'
completeCase (S.If s e1 e2 e3)        = do
  e1' <- completeCase e1
  e2' <- completeCase e2
  e3' <- completeCase e3
  return $ S.If s e1' e2' e3'
completeCase (S.Tuple s boxed es)     = do
  es' <- mapM completeCase es  -- complete case für List Expression
  return $ S.Tuple s boxed es'
completeCase (S.List s es)            = do
  es' <- mapM completeCase es
  return $ S.List s es'
completeCase (S.Paren s e)            = do
  e' <- completeCase e
  return $ S.Paren s e'
completeCase (S.ExpTypeSig s e t)     = do
  e' <- completeCase e
  return $ S.ExpTypeSig s e' t
-- Variables, Constructors and literals contain no expressions to complete
-- pattern matching in.
completeCase e@(S.Var _ _)            = return e
completeCase e@(S.Con _ _)            = return e
completeCase e@(S.Lit _ _)            = return e

completeBindRhs :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
                => S.Binds a
                -> Sem r (S.Binds a)
completeBindRhs (S.BDecls s decls) = do
  decls' <- mapM applyCCDecl decls
  return $ S.BDecls s decls'

getEqFromAlt :: Member Report r => S.Alt a -> Sem r (Eqs a)
getEqFromAlt (S.Alt _ pat rhs _) = do
  expr <- expFromUnguardedRhs rhs
  return ([pat], expr)

completeLambda :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
               => S.SrcSpan a
               -> [S.Pat a]
               -> S.Exp a
               -> Sem r (S.Exp a)
completeLambda s ps e = do
  let srcSpans = map S.getSrcSpan ps
  xs <- mapM (freshVarPatWithSrcSpan genericFreshPrefix) srcSpans
  e' <- completeCase e
  let eq = (ps, e')
  res <- match xs [eq] defaultErrorExp
  return $ S.Lambda s xs res

applyCCModule :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
              => S.Module a
              -> Sem r (S.Module a)
applyCCModule (S.Module s origModuleHead moduleName imports decls) = do
  decls' <- mapM applyCCDecl decls
  return $ S.Module s origModuleHead moduleName imports decls'

applyCCDecl :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
            => S.Decl a
            -> Sem r (S.Decl a)
applyCCDecl (S.FunBind s ms) = do
  nms <- applyCCMatches ms
  return (S.FunBind s nms)
applyCCDecl v                = return v

applyCCMatches :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
               => [S.Match a]
               -> Sem r [S.Match a]
applyCCMatches = mapM applyCCMatch
 where
  -- TODO maybe only apply if needed -> isIncomplete?
  applyCCMatch :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
               => S.Match a
               -> Sem r (S.Match a)
  applyCCMatch m = do
    let rhs     = S.matchRhs m
        srcSpan = S.getSrcSpan rhs
    expr <- expFromUnguardedRhs rhs
    expr' <- completeCase expr
    return $ m { S.matchRhs = S.UnGuardedRhs srcSpan expr' }
