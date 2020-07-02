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

import qualified Language.Haskell.Exts.Build   as B
import qualified Language.Haskell.Exts.Syntax  as HSE

type GExp = ([HSE.Pat ()], HSE.Rhs ())

-- Generates an expression with a let binding for each pattern + guard pair.
-- As defined in the semantics the first match of both pattern and guard has
-- to be evaluated causing a the sequential structure.
eliminateL
  :: [HSE.Pat ()]   -- fresh Vars
  -> HSE.Exp ()     -- error
  -> [GExp]     -- pairs of pattern and guarded rhs
  -> PM (HSE.Exp ())
eliminateL vs err eqs = do
  startVar         <- newVar
  (decls, lastPat) <- foldGEqs vs ([], startVar) eqs
  let errDecl = toDecl lastPat err -- error has to be bound to last new var
  return $ HSE.Let () (B.binds (errDecl : decls)) (A.translatePVar startVar)

toDecl :: HSE.Pat () -> HSE.Exp () -> HSE.Decl ()
toDecl p e = HSE.PatBind () p (HSE.UnGuardedRhs () e) B.noBinds

-- Folds the list of GExps to declarations.
foldGEqs
  :: [HSE.Pat ()]                   -- fresh variables for the case exps
  -> ([HSE.Decl ()], HSE.Pat ())    -- startcase ([], first generated Pattern)
  -> [GExp]                         -- list of pattern + rhs pair
  -> PM ([HSE.Decl ()], HSE.Pat ()) -- a list of declarations for the let binding and a
                                    -- free Variable for the error case
foldGEqs vs = foldM (\(decls, p) geq -> createDecl vs (decls, p) geq)

-- Generates a varbinding and a new variable for the next var binding
createDecl
  :: [HSE.Pat ()]                   -- generated varibles
  -> ([HSE.Decl ()], HSE.Pat ())    -- (current decls , variable for let binding)
  -> GExp                           -- pairs of pattern to match against and a guarded Rhs
  -> PM ([HSE.Decl ()], HSE.Pat ()) -- var bindings , variable for next match
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
  :: HSE.Exp ()             -- ifThenElse
  -> HSE.Exp ()             -- the other pattern (in case pattern match or guard fails)
  -> [(HSE.Pat (), HSE.Pat ())] -- Patterns to match (PVar , Pattern)
  -> HSE.Exp ()
-- createCase i next vps
--   = foldr (\(v,p) next ->
--       Case () (A.translatePVar v)
--               [B.alt p res, B.alt B.wildcard next]) i vps
createCase i _    []             = i
createCase i next ((v, p) : vps) = HSE.Case
  ()
  (A.translatePVar v)
  [B.alt p (createCase i next vps), B.alt B.wildcard next]

-- Converts a rhs into an if then else expression as mentioned in the semantics
rhsToIf
  :: HSE.Rhs ()      -- the (maybe guarded) righthandside
  -> HSE.Exp ()      -- next case
  -> PM (HSE.Exp ()) -- creates the if p_1 then . . . .
rhsToIf (HSE.UnGuardedRhs _ e   ) _    = applyGEExp e
rhsToIf (HSE.GuardedRhss  _ grhs) next = buildIF next grhs
 where
  buildIF
    :: HSE.Exp ()               -- next rule
    -> [HSE.GuardedRhs ()]      -- guarded rhs to fold
    -> PM (HSE.Exp ())          -- if then else expr
  buildIF nx gs = foldM
    (\res x -> extract x >>= \(e1, e2) -> return (HSE.If () e1 e2 res))
    nx
    (reverse gs) -- reverse, since foldM is a foldl with side effect

-- Converts a guarded rhs into a pair of a boolean expression and the right side
extract :: HSE.GuardedRhs () -> PM (HSE.Exp (), HSE.Exp ())
extract (HSE.GuardedRhs _ [s] e) = applyGEExp e
  >>= \a -> return (fromQualifier s, a)
 where
  fromQualifier :: HSE.Stmt () -> HSE.Exp ()
  fromQualifier (HSE.Qualifier () qe) = qe
  fromQualifier _                     = error "fromQualifier: no Qualifier"
-- TODO
extract (HSE.GuardedRhs _ (_ : _) _) =
  error "Currently only one guard exp allowed"
extract (HSE.GuardedRhs _ [] _) = error "GuardedRhss with no guards"

-- Applies guard elimination on an expression converting guarded rhs in cases
-- into unguarded exps
applyGEExp :: HSE.Exp () -> PM (HSE.Exp ())
applyGEExp e = case e of
  HSE.InfixApp _ e1 qop e2 -> do
    e1' <- applyGEExp e1
    e2' <- applyGEExp e2
    return $ HSE.InfixApp () e1' qop e2'
  HSE.App _ e1 e2 -> do
    e1' <- applyGEExp e1
    e2' <- applyGEExp e2
    return $ HSE.App () e1' e2'
  HSE.Lambda _ ps e1 -> do
    e' <- applyGEExp e1
    return $ HSE.Lambda () ps e'
  HSE.Let _ bs e1 -> do
    e' <- applyGEExp e1
    return $ HSE.Let () bs e'
  HSE.If _ e1 e2 e3 -> do
    e1' <- applyGEExp e1
    e2' <- applyGEExp e2
    e3' <- applyGEExp e3
    return $ HSE.If () e1' e2' e3'
  HSE.Case _ e1 alts -> do
    e'    <- applyGEExp e1
    alts' <- applyGEAlts alts
    return $ HSE.Case () e' alts'
  HSE.Tuple _ boxed es -> do
    es' <- mapM applyGEExp es
    return $ HSE.Tuple () boxed es'
  HSE.List _ es -> do
    es' <- mapM applyGEExp es
    return $ HSE.List () es'
  HSE.ListComp _ _ _ -> error "applyGEExp: ListComp not yet supported"
  -- can cause problems if a exp is missing in this case
  x                  -> return x

-- Applies guard elimination on alts by using eliminateL
applyGEAlts :: [HSE.Alt ()] -> PM [HSE.Alt ()]
applyGEAlts as = if any (\(HSE.Alt _ _ rhs _) -> isGuardedRhs rhs) as
  then do
    let gexps = map (\(HSE.Alt _ p rhs _) -> ([p], rhs)) as
    newVar'  <- newVar
    e        <- eliminateL [newVar'] A.err gexps
    matchVar <- newVar
    return [HSE.Alt () matchVar (HSE.UnGuardedRhs () e) B.noBinds]
  else return as

-- Applies guard elimination to a module
applyGEModule :: HSE.Module () -> PM (HSE.Module ())
applyGEModule (HSE.Module _ mmh mps ids ds) = do
  dcls <- mapM applyGEDecl ds
  return $ HSE.Module () mmh mps ids dcls
applyGEModule _ = error "applyGEModule: not on module"

-- Applies guard elimination to a declaration
applyGEDecl :: HSE.Decl () -> PM (HSE.Decl ())
applyGEDecl (HSE.FunBind _ ms) = do
  nms <- applyGEMatches ms
  return (HSE.FunBind () nms)
applyGEDecl v = return v

-- mapM
-- Applies guard elimination to a list of matches to generate one without guards
applyGEMatches :: [HSE.Match ()] -> PM [HSE.Match ()]
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
  :: [HSE.Match ()] -- one fun group
  -> PM (HSE.Match ())
applyGE ms = do
  let mname    = getMatchName ms
      geqs     = map (\(HSE.Match _ _ pats rhs _) -> (pats, rhs)) ms
      funArity = (length . fst . head) geqs
  nVars <- newVars funArity
  nExp  <- eliminateL nVars A.err geqs
  return $ HSE.Match () mname nVars (HSE.UnGuardedRhs () nExp) Nothing

-- compares the names of two matches
comp :: HSE.Match () -> HSE.Match () -> Bool
comp (HSE.Match _ name _ _ _) (HSE.Match _ name2 _ _ _) =
  selectNameStr name == selectNameStr name2
comp (HSE.InfixMatch _ _ name _ _ _) (HSE.InfixMatch _ _ name2 _ _ _) =
  selectNameStr name == selectNameStr name2
comp _ _ = False

selectNameStr :: HSE.Name () -> String
selectNameStr (HSE.Ident  _ str) = str
selectNameStr (HSE.Symbol _ str) = str

getMatchName :: [HSE.Match ()] -> HSE.Name ()
getMatchName [] = error "no match in getMatchName"
getMatchName ((HSE.Match _ mname _ _ _) : _) = mname
getMatchName ((HSE.InfixMatch _ _ mname _ _ _) : _) = mname


-- A function which determines if a group of Matches contains GuardedRhs
hasGuards
  :: [HSE.Match ()] -- one function
  -> Bool
hasGuards = any hasGuards'
 where
  hasGuards' :: HSE.Match () -> Bool
  hasGuards' (HSE.Match _ _ _ rhs _       ) = isGuardedRhs rhs
  hasGuards' (HSE.InfixMatch _ _ _ _ rhs _) = isGuardedRhs rhs

isGuardedRhs :: HSE.Rhs () -> Bool
isGuardedRhs (HSE.GuardedRhss  _ _) = True
isGuardedRhs (HSE.UnGuardedRhs _ e) = containsGuardedRhsExp e

-- TODO decide if guard is in matches or in matches
containsGuardedRhsExp :: HSE.Exp () -> Bool
containsGuardedRhsExp e = case e of
  HSE.InfixApp _ e1 _ e2 ->
    containsGuardedRhsExp e1 || containsGuardedRhsExp e2
  HSE.App    _ e1 e2 -> containsGuardedRhsExp e1 || containsGuardedRhsExp e2
  HSE.Lambda _ _  e' -> containsGuardedRhsExp e'
  HSE.Let    _ _  e' -> containsGuardedRhsExp e'
  HSE.If _ e1 e2 e3  -> any containsGuardedRhsExp [e1, e2, e3]
  HSE.Case _ e' alts ->
    containsGuardedRhsExp e' || any containsGuardedRhsAlt alts
  HSE.Tuple _ _ es -> any containsGuardedRhsExp es
  HSE.List _ es    -> any containsGuardedRhsExp es
  HSE.ListComp _ _ _ ->
    error "containsGuardedRhsExp: ListComp not yet supported"
  _ -> False

containsGuardedRhsAlt :: HSE.Alt () -> Bool
containsGuardedRhsAlt (HSE.Alt _ _ rhs _) = isGuardedRhs rhs
