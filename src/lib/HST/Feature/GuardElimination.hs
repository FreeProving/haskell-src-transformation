-- | This module contains methods for eliminating guards in modules.

module HST.Feature.GuardElimination
  ( eliminateL
  , applyGEModule
  , comp
  , getMatchName
  )
where                                   -- TODO Apply GE to GuardedRhs in case expressions
                                                                                -- TODO only apply to the parts with guards (not on matches if in case)
                                                                                    -- not false by semantics

import qualified HST.CoreAlgorithm             as A
                                                ( err
                                                , translatePVar
                                                )
import           Control.Monad                  ( foldM )
import           HST.Environment.FreshVars      ( PM
                                                , newVars
                                                , newVar
                                                )
import qualified HST.Frontend.Syntax           as S

type GExp s l t = ([S.Pat s l], S.Rhs s l t)

-- Generates an expression with a let binding for each pattern + guard pair.
-- As defined in the semantics the first match of both pattern and guard has
-- to be evaluated causing a the sequential structure.
eliminateL
  :: [S.Pat s l]  -- fresh Vars
  -> S.Exp s l t  -- error
  -> [GExp s l t] -- pairs of pattern and guarded rhs
  -> PM s l t (S.Exp s l t)
eliminateL vs err eqs = do
  startVar         <- newVar
  (decls, lastPat) <- foldGEqs vs ([], startVar) eqs
  let errDecl = toDecl lastPat err -- error has to be bound to last new var
  return $ S.Let S.NoSrcSpan
                 (S.BDecls S.NoSrcSpan (errDecl : decls))
                 (A.translatePVar startVar)

toDecl :: S.Pat s l -> S.Exp s l t -> S.Decl s l t
toDecl (S.PVar _ name) e = S.FunBind
  S.NoSrcSpan
  [S.Match S.NoSrcSpan name [] (S.UnGuardedRhs S.NoSrcSpan e) Nothing]
toDecl _ _ = error "GuardElimination.toDecl: Variable pattern expected"

-- Folds the list of GExps to declarations.
foldGEqs
  :: [S.Pat s l]                 -- fresh variables for the case exps
  -> ([S.Decl s l t], S.Pat s l) -- startcase ([], first generated Pattern)
  -> [GExp s l t]                -- list of pattern + rhs pair
  -> PM s l t ([S.Decl s l t], S.Pat s l) -- a list of declarations for the let binding
                                          -- and a free Variable for the error case
foldGEqs vs = foldM (\(decls, p) geq -> createDecl vs (decls, p) geq)

-- Generates a varbinding and a new variable for the next var binding
createDecl
  :: [S.Pat s l]                 -- generated varibles
  -> ([S.Decl s l t], S.Pat s l) -- (current decls , variable for let binding)
  -> GExp s l t                  -- pairs of pattern to match against and a guarded Rhs
  -> PM s l t ([S.Decl s l t], S.Pat s l) -- var bindings , variable for next match
createDecl vs (decl, p) (ps, rhs) = do
  nVar <- newVar
  let varExp = A.translatePVar nVar
  iexp <- rhsToIf rhs varExp
  let cexp  = createCase iexp varExp (zip vs ps)
  let ndecl = toDecl p cexp
  return (ndecl : decl, nVar)

-- TODO refactor to higher order
-- Generates a recursive case expression for each variable and pattern pair
createCase
  :: S.Exp s l t              -- ifThenElse
  -> S.Exp s l t              -- the other pattern (in case pattern match or guard fails)
  -> [(S.Pat s l, S.Pat s l)] -- Patterns to match (PVar , Pattern)
  -> S.Exp s l t
-- createCase i next vps
--   = foldr (\(v,p) next ->
--       Case () (A.translatePVar v)
--               [S.alt p res, S.alt B.wildcard next]) i vps
createCase i _    []             = i
createCase i next ((v, p) : vps) = S.Case
  S.NoSrcSpan
  (A.translatePVar v)
  [S.alt p (createCase i next vps), S.alt (S.PWildCard S.NoSrcSpan) next]

-- Converts a rhs into an if then else expression as mentioned in the semantics
rhsToIf
  :: S.Rhs s l t      -- the (maybe guarded) righthandside
  -> S.Exp s l t      -- next case
  -> PM s l t (S.Exp s l t) -- creates the if p_1 then . . . .
rhsToIf (S.UnGuardedRhs _ e   ) _    = applyGEExp e
rhsToIf (S.GuardedRhss  _ grhs) next = buildIF next grhs
 where
  buildIF
    :: S.Exp s l t               -- next rule
    -> [S.GuardedRhs s l t]      -- guarded rhs to fold
    -> PM s l t (S.Exp s l t)    -- if then else expr
  buildIF nx gs = foldM
    (\res (S.GuardedRhs _ e1 e2) -> return (S.If S.NoSrcSpan e1 e2 res))
    nx
    (reverse gs) -- reverse, since foldM is a foldl with side effect

-- Applies guard elimination on an expression converting guarded rhs in cases
-- into unguarded exps
applyGEExp :: S.Exp s l t -> PM s l t (S.Exp s l t)
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
applyGEAlts :: [S.Alt s l t] -> PM s l t [S.Alt s l t]
applyGEAlts as = if any (\(S.Alt _ _ rhs _) -> isGuardedRhs rhs) as
  then do
    let gexps = map (\(S.Alt _ p rhs _) -> ([p], rhs)) as
    newVar'  <- newVar
    e        <- eliminateL [newVar'] A.err gexps
    matchVar <- newVar
    return [S.Alt S.NoSrcSpan matchVar (S.UnGuardedRhs S.NoSrcSpan e) Nothing]
  else return as

-- Applies guard elimination to a module
applyGEModule :: S.Module s l t -> PM s l t (S.Module s l t)
applyGEModule (S.Module ds) = do
  dcls <- mapM applyGEDecl ds
  return $ S.Module dcls

-- Applies guard elimination to a declaration
applyGEDecl :: S.Decl s l t -> PM s l t (S.Decl s l t)
applyGEDecl (S.FunBind _ ms) = do
  nms <- applyGEMatches ms
  return (S.FunBind S.NoSrcSpan nms)
applyGEDecl v = return v

-- mapM
-- Applies guard elimination to a list of matches to generate one without guards
applyGEMatches :: [S.Match s l t] -> PM s l t [S.Match s l t]
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
  :: [S.Match s l t] -- one fun group
  -> PM s l t (S.Match s l t)
applyGE ms = do
  let mname    = getMatchName ms
      geqs     = map (\(S.Match _ _ pats rhs _) -> (pats, rhs)) ms
      funArity = (length . fst . head) geqs
  nVars <- newVars funArity
  nExp  <- eliminateL nVars A.err geqs
  return $ S.Match S.NoSrcSpan
                   mname
                   nVars
                   (S.UnGuardedRhs S.NoSrcSpan nExp)
                   Nothing

-- compares the names of two matches
comp :: S.Match s l t -> S.Match s l t -> Bool
comp (S.Match _ name _ _ _) (S.Match _ name2 _ _ _) =
  selectNameStr name == selectNameStr name2
comp (S.InfixMatch _ _ name _ _ _) (S.InfixMatch _ _ name2 _ _ _) =
  selectNameStr name == selectNameStr name2
comp _ _ = False

selectNameStr :: S.Name s -> String
selectNameStr (S.Ident  _ str) = str
selectNameStr (S.Symbol _ str) = str

getMatchName :: [S.Match s l t] -> S.Name s
getMatchName [] = error "no match in getMatchName"
getMatchName ((S.Match _ mname _ _ _) : _) = mname
getMatchName ((S.InfixMatch _ _ mname _ _ _) : _) = mname


-- A function which determines if a group of Matches contains GuardedRhs
hasGuards
  :: [S.Match s l t] -- one function
  -> Bool
hasGuards = any hasGuards'
 where
  hasGuards' :: S.Match s l t -> Bool
  hasGuards' (S.Match _ _ _ rhs _       ) = isGuardedRhs rhs
  hasGuards' (S.InfixMatch _ _ _ _ rhs _) = isGuardedRhs rhs

isGuardedRhs :: S.Rhs s l t -> Bool
isGuardedRhs (S.GuardedRhss  _ _) = True
isGuardedRhs (S.UnGuardedRhs _ e) = containsGuardedRhsExp e

-- TODO decide if guard is in matches or in matches
containsGuardedRhsExp :: S.Exp s l t -> Bool
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

containsGuardedRhsAlt :: S.Alt s l t -> Bool
containsGuardedRhsAlt (S.Alt _ _ rhs _) = isGuardedRhs rhs
