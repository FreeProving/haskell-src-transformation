-- | This module contains methods for eliminating guards in modules.

module HST.Feature.GuardElimination
  ( applyGEModule
  , getMatchName
  )
where

import           Control.Monad                  ( foldM
                                                , replicateM
                                                )
import           Polysemy                       ( Member
                                                , Members
                                                , Sem
                                                )

import           HST.CoreAlgorithm              ( defaultErrorExp
                                                , translatePVar
                                                )
import           HST.Effect.Fresh               ( Fresh
                                                , freshName
                                                , freshVarPat
                                                , genericFreshPrefix
                                                )
import qualified HST.Frontend.Syntax           as S

-- | A pair of patterns to match and a right-hand side.
type GExp a = ([S.Pat a], S.Rhs a)

-- | Converts a rule of a function declaration to a 'GExp'.
matchToGExp :: S.Match a -> GExp a
matchToGExp (S.Match _ _ pats rhs _         ) = (pats, rhs)
matchToGExp (S.InfixMatch _ pat _ pats rhs _) = (pat : pats, rhs)

-- | Converts an alternative of a @case@ expression to a 'GExp'.
altToGExp :: S.Alt a -> GExp a
altToGExp (S.Alt _ p rhs _) = ([p], rhs)

-------------------------------------------------------------------------------
-- @let@ Expressions                                                         --
-------------------------------------------------------------------------------

-- Generates an expression with a let binding for each pattern + guard pair.
-- As defined in the semantics the first match of both pattern and guard has
-- to be evaluated causing a the sequential structure.
eliminateL
  :: Members '[Fresh] r
  => [S.Exp a] -- fresh Vars
  -> S.Exp a   -- error
  -> [GExp a]  -- pairs of pattern and guarded rhs
  -> Sem r (S.Exp a)
eliminateL vs err eqs = do
  startVar         <- freshName genericFreshPrefix
  (decls, lastPat) <- foldGEqs vs ([], startVar) eqs
  let errDecl = makeVarBinding lastPat err -- error has to be bound to last new var
  return $ S.Let S.NoSrcSpan
                 (S.BDecls S.NoSrcSpan (errDecl : decls))
                 (S.var startVar)

-- | Creates a function declaration for a @let@ binding of a variable with the
--   given name to the given expression.
makeVarBinding :: S.Name a -> S.Exp a -> S.Decl a
makeVarBinding name e = S.FunBind
  S.NoSrcSpan
  [S.Match S.NoSrcSpan name [] (S.UnGuardedRhs S.NoSrcSpan e) Nothing]

-- Folds the list of GExps to declarations.
foldGEqs
  :: Member Fresh r
  => [S.Exp a]                    -- fresh variables for the case exps
  -> ([S.Decl a], S.Name a)       -- startcase ([], first generated Pattern)
  -> [GExp a]                     -- list of pattern + rhs pair
  -> Sem r ([S.Decl a], S.Name a) -- a list of declarations for the let binding
                                  -- and a free Variable for the error case
foldGEqs vs = foldM (createDecl vs)

-- Generates a varbinding and a new variable for the next var binding
createDecl
  :: Member Fresh r
  => [S.Exp a]                    -- generated variables
  -> ([S.Decl a], S.Name a)       -- (current decls , variable for let binding)
  -> GExp a                       -- pairs of pattern to match against and a guarded Rhs
  -> Sem r ([S.Decl a], S.Name a) -- var bindings , variable for next match
createDecl vs (decls, varName) (ps, rhs) = do
  nextVarName <- freshName genericFreshPrefix
  let nextVarExp = S.var nextVarName
  iexp <- rhsToIf rhs nextVarExp
  let cexp  = generateNestedCases iexp nextVarExp (zip vs ps)
  let ndecl = makeVarBinding varName cexp
  return (ndecl : decls, nextVarName)

-------------------------------------------------------------------------------
-- @case@ Expressions                                                        --
-------------------------------------------------------------------------------

-- | Generates nested case expression for each variable and pattern pair.
generateNestedCases
  :: S.Exp a              -- ^ Expression to use if all pattern match.
  -> S.Exp a              -- ^ Expression to use if any pattern does not match.
  -> [(S.Exp a, S.Pat a)] -- ^ Expression/pattern pairs to match.
  -> S.Exp a
generateNestedCases successExpr failExpr = foldr generateNestedCase successExpr
 where
  {- generateNestedCase :: (S.Exp a, S.Pat a) -> S.Exp a -> S.Exp a -}
  generateNestedCase (v, p) nestedExpr =
    S.Case S.NoSrcSpan v
      $ [S.alt p nestedExpr, S.alt (S.PWildCard S.NoSrcSpan) failExpr]

-------------------------------------------------------------------------------
-- @if@ Expressions                                                          --
-------------------------------------------------------------------------------

-- | Converts a right-hand side to an @if@ expression.
--
--    TODO Guards in unguarded expressions are eliminated recursively. But why
--    arent't guards in guarded right-hand sides eliminated recursively?
rhsToIf
  :: Member Fresh r
  => S.Rhs a         -- ^ The right-hand side to convert.
  -> S.Exp a         -- ^ The next expression if no guard matches.
  -> Sem r (S.Exp a)
rhsToIf (S.UnGuardedRhs _ e   ) _    = applyGEExp e
rhsToIf (S.GuardedRhss  _ grhs) next = return (foldr guardedRhsToIf next grhs)

-- | Converts a guarded right-hand side to an @if@ expression.
--
--   The condition of the guard is used as the condition of the @if@
--   expression and the expression on the right-hand side is used as
--   the @then@ branch. The second argument is the @else@ branch.
guardedRhsToIf :: S.GuardedRhs a -> S.Exp a -> S.Exp a
guardedRhsToIf (S.GuardedRhs _ e1 e2) = S.If S.NoSrcSpan e1 e2

-------------------------------------------------------------------------------
-- Guard Elimination                                                         --
-------------------------------------------------------------------------------

-- | Applies guard elimination on @case@ expression in the given expression.
applyGEExp :: Member Fresh r => S.Exp a -> Sem r (S.Exp a)
applyGEExp (S.InfixApp _ e1 qop e2) = do
  e1' <- applyGEExp e1
  e2' <- applyGEExp e2
  return $ S.InfixApp S.NoSrcSpan e1' qop e2'
applyGEExp (S.NegApp _ expr) = do
  expr' <- applyGEExp expr
  return $ S.NegApp S.NoSrcSpan expr'
applyGEExp (S.App _ e1 e2) = do
  e1' <- applyGEExp e1
  e2' <- applyGEExp e2
  return $ S.App S.NoSrcSpan e1' e2'
applyGEExp (S.Lambda _ ps e1) = do
  e' <- applyGEExp e1
  return $ S.Lambda S.NoSrcSpan ps e'
applyGEExp (S.Let _ bs e1) = do
  e' <- applyGEExp e1
  return $ S.Let S.NoSrcSpan bs e'
applyGEExp (S.If _ e1 e2 e3) = do
  e1' <- applyGEExp e1
  e2' <- applyGEExp e2
  e3' <- applyGEExp e3
  return $ S.If S.NoSrcSpan e1' e2' e3'
applyGEExp (S.Case _ e1 alts) = do
  e'    <- applyGEExp e1
  alts' <- applyGEAlts alts
  return $ S.Case S.NoSrcSpan e' alts'
applyGEExp (S.Tuple _ boxed es) = do
  es' <- mapM applyGEExp es
  return $ S.Tuple S.NoSrcSpan boxed es'
applyGEExp (S.List _ es) = do
  es' <- mapM applyGEExp es
  return $ S.List S.NoSrcSpan es'
applyGEExp (S.Paren _ expr) = do
  expr' <- applyGEExp expr
  return $ S.Paren S.NoSrcSpan expr'
applyGEExp (S.ExpTypeSig _ expr typeExpr) = do
  expr' <- applyGEExp expr
  return $ S.ExpTypeSig S.NoSrcSpan expr' typeExpr
-- Variables, constructors and literals remain unchanged.
applyGEExp e@(S.Var _ _) = return e
applyGEExp e@(S.Con _ _) = return e
applyGEExp e@(S.Lit _ _) = return e

-- | Applies guard elimination on @case@ expression alternatives.
applyGEAlts :: Member Fresh r => [S.Alt a] -> Sem r [S.Alt a]
applyGEAlts alts
  | any hasGuardsAlt alts = do
    let gexps = map altToGExp alts
    newVar'  <- freshVarPat genericFreshPrefix
    e        <- eliminateL [translatePVar newVar'] defaultErrorExp gexps
    return [S.Alt S.NoSrcSpan newVar' (S.UnGuardedRhs S.NoSrcSpan e) Nothing]
  | otherwise = return alts

-- | Applies guard elimination to function declarations in the given module.
applyGEModule :: Member Fresh r => S.Module a -> Sem r (S.Module a)
applyGEModule (S.Module decls) = do
  decls' <- mapM applyGEDecl decls
  return $ S.Module decls'

-- | Applies guard elimination to a declaration.
--
--   Non-function declarations are returned unchanged.
applyGEDecl :: Member Fresh r => S.Decl a -> Sem r (S.Decl a)
applyGEDecl (S.FunBind _ ms) = do
  ms' <- applyGEMatches ms
  return (S.FunBind S.NoSrcSpan ms')
applyGEDecl decl@(S.DataDecl _ _) = return decl

-- | Applies guard elimination to the rules of a function declaration.
--
--   If no rule of the function has guards and there are no subexpressions
--   with guards, the function is left unchanged.
--
--   TODO only apply to the parts with guards (not on matches if in case)
--        not false by semantics
applyGEMatches :: Member Fresh r => [S.Match a] -> Sem r [S.Match a]
applyGEMatches ms | any hasGuards ms = return <$> applyGE ms
                  | otherwise        = return ms

-- | Applies guard elimination to the rules of a function declaration.
applyGE :: Member Fresh r => [S.Match a] -> Sem r (S.Match a)
applyGE ms = do
  let mname    = getMatchName ms
      geqs     = map matchToGExp ms
      funArity = (length . fst . head) geqs
  nVars <- replicateM funArity (freshVarPat genericFreshPrefix)
  nExp  <- eliminateL (map translatePVar nVars) defaultErrorExp geqs
  return $ S.Match S.NoSrcSpan
                   mname
                   nVars
                   (S.UnGuardedRhs S.NoSrcSpan nExp)
                   Nothing

-- | Gets the name of the function that is defined by the given rules.
getMatchName :: [S.Match a] -> S.Name a
getMatchName [] = error "no match in getMatchName"
getMatchName (S.Match _ mname _ _ _ : _) = mname
getMatchName (S.InfixMatch _ _ mname _ _ _ : _) = mname

-------------------------------------------------------------------------------
-- Predicates                                                                --
-------------------------------------------------------------------------------

-- | Tests whether the given rule of a function declaration uses
--   guards or contains an expression with guards.
hasGuards :: S.Match a -> Bool
hasGuards (S.Match _ _ _ rhs _       ) = hasGuardsRhs rhs
hasGuards (S.InfixMatch _ _ _ _ rhs _) = hasGuardsRhs rhs

-- | Tests whether the given right-hand side of a function rule has a guard
--   itself or contains an expression that has subexpressions with guarded
--   right-hand sides.
hasGuardsRhs :: S.Rhs a -> Bool
hasGuardsRhs (S.GuardedRhss  _ _) = True
hasGuardsRhs (S.UnGuardedRhs _ e) = hasGuardsExp e

-- | Tests whether the given expression has a subexpression with guarded
--   right-hand sides.
hasGuardsExp :: S.Exp a -> Bool
hasGuardsExp (S.InfixApp _ e1 _ e2) = hasGuardsExp e1 || hasGuardsExp e2
hasGuardsExp (S.NegApp _ e'       ) = hasGuardsExp e'
hasGuardsExp (S.App    _ e1 e2    ) = hasGuardsExp e1 || hasGuardsExp e2
hasGuardsExp (S.Lambda _ _  e'    ) = hasGuardsExp e'
hasGuardsExp (S.Let    _ _  e'    ) = hasGuardsExp e'
hasGuardsExp (S.If _ e1 e2 e3     ) = any hasGuardsExp [e1, e2, e3]
hasGuardsExp (S.Case  _ e' alts   ) = hasGuardsExp e' || any hasGuardsAlt alts
hasGuardsExp (S.Tuple _ _  es     ) = any hasGuardsExp es
hasGuardsExp (S.List  _ es        ) = any hasGuardsExp es
hasGuardsExp (S.Paren _ e'        ) = hasGuardsExp e'
hasGuardsExp (S.ExpTypeSig _ e' _ ) = hasGuardsExp e'
  -- Variables, constructors and literals contain no guards.
hasGuardsExp (S.Var _ _           ) = False
hasGuardsExp (S.Con _ _           ) = False
hasGuardsExp (S.Lit _ _           ) = False

-- | Tests whether the right-hand side of the given alternative of a @case@
--   expression has a guard or a subexpression with guards.
hasGuardsAlt :: S.Alt a -> Bool
hasGuardsAlt (S.Alt _ _ rhs _) = hasGuardsRhs rhs
