-- | This module contains methods for eliminating guards in modules.

module HST.Feature.GuardElimination
  ( eliminateL
  , applyGEModule
  , comp
  , getMatchName
  )
where

-- TODO Apply GE to GuardedRhs in case expressions
-- TODO only apply to the parts with guards (not on matches if in case)
--      not false by semantics

import           Control.Monad                  ( foldM
                                                , replicateM
                                                )
import           Polysemy                       ( Member
                                                , Members
                                                , Sem
                                                )

import qualified HST.CoreAlgorithm             as CA
import           HST.Effect.Fresh               ( Fresh
                                                , freshVarPat
                                                , genericFreshPrefix
                                                )
import qualified HST.Frontend.Syntax           as S

type GExp a = ([S.Pat a], S.Rhs a)

-- Generates an expression with a let binding for each pattern + guard pair.
-- As defined in the semantics the first match of both pattern and guard has
-- to be evaluated causing a the sequential structure.
eliminateL
  :: Members '[Fresh] r
  => [S.Pat a] -- fresh Vars
  -> S.Exp a   -- error
  -> [GExp a]  -- pairs of pattern and guarded rhs
  -> Sem r (S.Exp a)
eliminateL vs err eqs = do
  startVar         <- freshVarPat genericFreshPrefix
  (decls, lastPat) <- foldGEqs vs ([], startVar) eqs
  let errDecl = toDecl lastPat err -- error has to be bound to last new var
  return $ S.Let S.NoSrcSpan
                 (S.BDecls S.NoSrcSpan (errDecl : decls))
                 (CA.translatePVar startVar)

toDecl :: S.Pat a -> S.Exp a -> S.Decl a
toDecl (S.PVar _ name) e = S.FunBind
  S.NoSrcSpan
  [S.Match S.NoSrcSpan name [] (S.UnGuardedRhs S.NoSrcSpan e) Nothing]
toDecl _ _ = error "GuardElimination.toDecl: Variable pattern expected"

-- Folds the list of GExps to declarations.
foldGEqs
  :: Member Fresh r
  => [S.Pat a]                   -- fresh variables for the case exps
  -> ([S.Decl a], S.Pat a)       -- startcase ([], first generated Pattern)
  -> [GExp a]                    -- list of pattern + rhs pair
  -> Sem r ([S.Decl a], S.Pat a) -- a list of declarations for the let binding
                                          -- and a free Variable for the error case
foldGEqs vs = foldM (createDecl vs)

-- Generates a varbinding and a new variable for the next var binding
createDecl
  :: Member Fresh r
  => [S.Pat a]                   -- generated variables
  -> ([S.Decl a], S.Pat a)       -- (current decls , variable for let binding)
  -> GExp a                      -- pairs of pattern to match against and a guarded Rhs
  -> Sem r ([S.Decl a], S.Pat a) -- var bindings , variable for next match
createDecl vs (decl, p) (ps, rhs) = do
  nVar <- freshVarPat genericFreshPrefix
  let varExp = CA.translatePVar nVar
  iexp <- rhsToIf rhs varExp
  let cexp  = createCase iexp varExp (zip vs ps)
  let ndecl = toDecl p cexp
  return (ndecl : decl, nVar)

-- TODO refactor to higher order
-- Generates a recursive case expression for each variable and pattern pair
createCase
  :: S.Exp a              -- ifThenElse
  -> S.Exp a              -- the other pattern (in case pattern match or guard fails)
  -> [(S.Pat a, S.Pat a)] -- Patterns to match (PVar , Pattern)
  -> S.Exp a
-- createCase i next vps
--   = foldr (\(v,p) next ->
--       Case () (translatePVar v)
--               [S.alt p res, S.alt B.wildcard next]) i vps
createCase i _    []             = i
createCase i next ((v, p) : vps) = S.Case
  S.NoSrcSpan
  (CA.translatePVar v)
  [S.alt p (createCase i next vps), S.alt (S.PWildCard S.NoSrcSpan) next]

-- Converts a rhs into an if then else expression as mentioned in the semantics
rhsToIf
  :: Member Fresh r
  => S.Rhs a         -- the (maybe guarded) right hand side
  -> S.Exp a         -- next case
  -> Sem r (S.Exp a) -- creates the if p_1 then . . . .
rhsToIf (S.UnGuardedRhs _ e   ) _    = applyGEExp e
rhsToIf (S.GuardedRhss  _ grhs) next = buildIf next grhs
 where
  buildIf
    :: S.Exp a          -- next rule
    -> [S.GuardedRhs a] -- guarded rhs to fold
    -> Sem r (S.Exp a)  -- if then else expr
  buildIf nx gs = foldM
    (\res (S.GuardedRhs _ e1 e2) -> return (S.If S.NoSrcSpan e1 e2 res))
    nx
    (reverse gs) -- reverse, since foldM is a foldl with side effect

-- Applies guard elimination on an expression converting guarded rhs in cases
-- into unguarded exps
applyGEExp :: Member Fresh r => S.Exp a -> Sem r (S.Exp a)
applyGEExp e = case e of
  S.InfixApp _ e1 qop e2 -> do
    e1' <- applyGEExp e1
    e2' <- applyGEExp e2
    return $ S.InfixApp S.NoSrcSpan e1' qop e2'
  S.App _ e1 e2 -> do
    e1' <- applyGEExp e1
    e2' <- applyGEExp e2
    return $ S.App S.NoSrcSpan e1' e2'
  S.Lambda _ ps e1 -> do
    e' <- applyGEExp e1
    return $ S.Lambda S.NoSrcSpan ps e'
  S.Let _ bs e1 -> do
    e' <- applyGEExp e1
    return $ S.Let S.NoSrcSpan bs e'
  S.If _ e1 e2 e3 -> do
    e1' <- applyGEExp e1
    e2' <- applyGEExp e2
    e3' <- applyGEExp e3
    return $ S.If S.NoSrcSpan e1' e2' e3'
  S.Case _ e1 alts -> do
    e'    <- applyGEExp e1
    alts' <- applyGEAlts alts
    return $ S.Case S.NoSrcSpan e' alts'
  S.Tuple _ boxed es -> do
    es' <- mapM applyGEExp es
    return $ S.Tuple S.NoSrcSpan boxed es'
  S.List _ es -> do
    es' <- mapM applyGEExp es
    return $ S.List S.NoSrcSpan es'
  -- can cause problems if a exp is missing in this case
  x -> return x

-- Applies guard elimination on alts by using eliminateL
applyGEAlts :: Member Fresh r => [S.Alt a] -> Sem r [S.Alt a]
applyGEAlts as = if any (\(S.Alt _ _ rhs _) -> isGuardedRhs rhs) as
  then do
    let gexps = map (\(S.Alt _ p rhs _) -> ([p], rhs)) as
    newVar'  <- freshVarPat genericFreshPrefix
    e        <- eliminateL [newVar'] CA.err gexps
    matchVar <- freshVarPat genericFreshPrefix
    return [S.Alt S.NoSrcSpan matchVar (S.UnGuardedRhs S.NoSrcSpan e) Nothing]
  else return as

-- Applies guard elimination to a module
applyGEModule :: Member Fresh r => S.Module a -> Sem r (S.Module a)
applyGEModule (S.Module ds) = do
  dcls <- mapM applyGEDecl ds
  return $ S.Module dcls

-- Applies guard elimination to a declaration
applyGEDecl :: Member Fresh r => S.Decl a -> Sem r (S.Decl a)
applyGEDecl (S.FunBind _ ms) = do
  nms <- applyGEMatches ms
  return (S.FunBind S.NoSrcSpan nms)
applyGEDecl v = return v

-- mapM
-- Applies guard elimination to a list of matches to generate one without guards
applyGEMatches :: Member Fresh r => [S.Match a] -> Sem r [S.Match a]
applyGEMatches []       = return []
applyGEMatches (m : ms) = do
  let (oneFun, r) = span (comp m) ms
      funGroup    = m : oneFun
  if hasGuards funGroup
    then do
      x  <- applyGE funGroup
      xs <- applyGEMatches r
      return (x : xs)
    else do
      xs <- applyGEMatches r
      return (funGroup ++ xs)

-- Applies guard elimination to one function
applyGE
  :: Member Fresh r
  => [S.Match a] -- one fun group
  -> Sem r (S.Match a)
applyGE ms = do
  let mname    = getMatchName ms
      geqs     = map (\(S.Match _ _ pats rhs _) -> (pats, rhs)) ms
      funArity = (length . fst . head) geqs
  nVars <- replicateM funArity (freshVarPat genericFreshPrefix)
  nExp  <- eliminateL nVars CA.err geqs
  return $ S.Match S.NoSrcSpan
                   mname
                   nVars
                   (S.UnGuardedRhs S.NoSrcSpan nExp)
                   Nothing

-- compares the names of two matches
comp :: S.Match a -> S.Match a -> Bool
comp (S.Match _ name _ _ _) (S.Match _ name2 _ _ _) =
  selectNameStr name == selectNameStr name2
comp (S.InfixMatch _ _ name _ _ _) (S.InfixMatch _ _ name2 _ _ _) =
  selectNameStr name == selectNameStr name2
comp _ _ = False

selectNameStr :: S.Name a -> String
selectNameStr (S.Ident  _ str) = str
selectNameStr (S.Symbol _ str) = str

getMatchName :: [S.Match a] -> S.Name a
getMatchName [] = error "no match in getMatchName"
getMatchName ((S.Match _ mname _ _ _) : _) = mname
getMatchName ((S.InfixMatch _ _ mname _ _ _) : _) = mname


-- A function which determines if a group of Matches contains GuardedRhs
hasGuards
  :: [S.Match a] -- one function
  -> Bool
hasGuards = any hasGuards'
 where
  hasGuards' :: S.Match a -> Bool
  hasGuards' (S.Match _ _ _ rhs _       ) = isGuardedRhs rhs
  hasGuards' (S.InfixMatch _ _ _ _ rhs _) = isGuardedRhs rhs

isGuardedRhs :: S.Rhs a -> Bool
isGuardedRhs (S.GuardedRhss  _ _) = True
isGuardedRhs (S.UnGuardedRhs _ e) = containsGuardedRhsExp e

-- TODO decide if guard is in matches or in matches
containsGuardedRhsExp :: S.Exp a -> Bool
containsGuardedRhsExp e = case e of
  S.InfixApp _ e1 _ e2 -> containsGuardedRhsExp e1 || containsGuardedRhsExp e2
  S.App    _ e1 e2     -> containsGuardedRhsExp e1 || containsGuardedRhsExp e2
  S.Lambda _ _  e'     -> containsGuardedRhsExp e'
  S.Let    _ _  e'     -> containsGuardedRhsExp e'
  S.If _ e1 e2 e3      -> any containsGuardedRhsExp [e1, e2, e3]
  S.Case _ e' alts ->
    containsGuardedRhsExp e' || any containsGuardedRhsAlt alts
  S.Tuple _ _ es -> any containsGuardedRhsExp es
  S.List _ es    -> any containsGuardedRhsExp es
  _              -> False

containsGuardedRhsAlt :: S.Alt a -> Bool
containsGuardedRhsAlt (S.Alt _ _ rhs _) = isGuardedRhs rhs
