-- | This module contains methods for completing case expressions both in
--   expressions and entire modules.

module HST.Feature.CaseCompletion
  ( applyCCModule
  )
where

import           Control.Monad                  ( replicateM )
import           Polysemy                       ( Member
                                                , Members
                                                , Sem
                                                )

import           HST.CoreAlgorithm              ( match
                                                , defaultErrorExp
                                                , Eqs
                                                )
import           HST.Effect.Env                 ( Env )
import           HST.Effect.Fresh               ( Fresh
                                                , freshVarPat
                                                , genericFreshPrefix
                                                )
import           HST.Effect.GetOpt              ( GetOpt )
import           HST.Effect.Report              ( Report )
import qualified HST.Frontend.Syntax           as S
import           HST.Util.Selectors             ( expFromUnguardedRhs )

-- | Takes a given expression and applies the algorithm on it resulting in
--   completed cases
completeCase
  :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
  => Bool
  -> S.Exp a
  -> Sem r (S.Exp a)
completeCase insideLet (S.Case _ expr as) = do
  v    <- freshVarPat genericFreshPrefix
  eqs  <- mapM getEqFromAlt as
  eqs' <- mapM (\(p, ex) -> completeCase insideLet ex >>= \e -> return (p, e))
               eqs
  res <- match [v] eqs' defaultErrorExp
  if not insideLet
    then do
      let (S.Case _ _ resAs) = res
      return $ S.Case S.NoSrcSpan expr resAs  -- test to deconstruct the first case
    else do
      let a = S.alt v res
      return $ S.Case S.NoSrcSpan expr [a]
completeCase il (S.InfixApp _ e1 qop e2) = do
  e1' <- completeCase il e1
  e2' <- completeCase il e2
  return $ S.InfixApp S.NoSrcSpan e1' qop e2'
completeCase il (S.NegApp _ e) = do
  e' <- completeCase il e
  return $ S.NegApp S.NoSrcSpan e'
completeCase il (S.App _ e1 e2) = do
  e1' <- completeCase il e1
  e2' <- completeCase il e2
  return $ S.App S.NoSrcSpan e1' e2'
completeCase il (S.Lambda _ ps    e) = completeLambda ps e il
completeCase il (S.Let    _ binds e) = do
  e'     <- completeCase il e -- undefined -- TODO
  binds' <- completeBindRhs binds
  return $ S.Let S.NoSrcSpan binds' e'
completeCase il (S.If _ e1 e2 e3) = do
  e1' <- completeCase il e1
  e2' <- completeCase il e2
  e3' <- completeCase il e3
  return $ S.If S.NoSrcSpan e1' e2' e3'
completeCase il (S.Tuple _ boxed es) = do
  es' <- mapM (completeCase il) es  -- complete case für List Expression
  return $ S.Tuple S.NoSrcSpan boxed es'
completeCase il (S.List _ es) = do
  es' <- mapM (completeCase il) es
  return $ S.List S.NoSrcSpan es'
completeCase il (S.Paren _ e) = do
  e' <- completeCase il e
  return $ S.Paren S.NoSrcSpan e'
completeCase il (S.ExpTypeSig _ e t) = do
  e' <- completeCase il e
  return $ S.ExpTypeSig S.NoSrcSpan e' t

-- Variables, Constructors and literals contain no expressions to complete
-- pattern matching in.
completeCase _ e@(S.Var _ _) = return e
completeCase _ e@(S.Con _ _) = return e
completeCase _ e@(S.Lit _ _) = return e

completeBindRhs
  :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
  => S.Binds a
  -> Sem r (S.Binds a)
completeBindRhs (S.BDecls _ decls) = do
  decls' <- mapM (applyCCDecl True) decls
  return $ S.BDecls S.NoSrcSpan decls'

getEqFromAlt :: Member Report r => S.Alt a -> Sem r (Eqs a)
getEqFromAlt (S.Alt _ pat rhs _) = do
  expr <- expFromUnguardedRhs rhs
  return ([pat], expr)

completeLambda
  :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
  => [S.Pat a]
  -> S.Exp a
  -> Bool
  -> Sem r (S.Exp a)
completeLambda ps e insideLet = do
  xs <- replicateM (length ps) (freshVarPat genericFreshPrefix)
  e' <- completeCase insideLet e
  let eq = (ps, e')
  res <- match xs [eq] defaultErrorExp
  return $ S.Lambda S.NoSrcSpan xs res

applyCCModule
  :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
  => S.Module a
  -> Sem r (S.Module a)
applyCCModule (S.Module s origModuleHead decls) = do
  decls' <- mapM (applyCCDecl False) decls
  return $ S.Module s origModuleHead decls'

applyCCDecl
  :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
  => Bool
  -> S.Decl a
  -> Sem r (S.Decl a)
applyCCDecl insideLet (S.FunBind _ ms) = do
  nms <- applyCCMatches insideLet ms
  return (S.FunBind S.NoSrcSpan nms)
applyCCDecl _ v = return v

applyCCMatches
  :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
  => Bool
  -> [S.Match a]
  -> Sem r [S.Match a]
applyCCMatches insideLet = mapM applyCCMatch
 where
  -- TODO maybe only apply if needed -> isIncomplete?
  applyCCMatch
    :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
    => S.Match a
    -> Sem r (S.Match a)
  applyCCMatch (S.Match _ n ps rhs _) = do
    e <- expFromUnguardedRhs rhs
    x <- completeCase insideLet e
    return $ S.Match S.NoSrcSpan n ps (S.UnGuardedRhs S.NoSrcSpan x) Nothing
  applyCCMatch (S.InfixMatch _ p n ps rhs _) = do
    e <- expFromUnguardedRhs rhs
    x <- completeCase insideLet e
    return
      $ S.InfixMatch S.NoSrcSpan p n ps (S.UnGuardedRhs S.NoSrcSpan x) Nothing
