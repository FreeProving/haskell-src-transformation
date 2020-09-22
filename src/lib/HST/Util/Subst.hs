-- | This module contains an abstract data type for the substitution of
--   variables by expressions and a type class for the application of such
--   substitutions to more complex AST nodes.
module HST.Util.Subst
  ( -- * Substitutions
    Subst
    -- * Construction
  , identitySubst
  , singleSubst
  , substFromList
    -- * Composition
  , composeSubst
  , composeSubsts
  , extendSubst
    -- * Application
  , ApplySubst(..)
  ) where

import           Data.Char           ( isDigit )
import           Data.Composition    ( (.:) )
import           Data.List.Extra     ( breakOnEnd, dropEnd1 )
import           Data.Map.Strict     ( Map )
import qualified Data.Map.Strict     as Map
import           Data.Set            ( Set )
import qualified Data.Set            as Set
import           Data.Tuple.Extra    ( (***) )

import qualified HST.Frontend.Syntax as S
import           HST.Util.FreeVars
  ( BoundVars, FreeVars, boundVars, freeVarSet, withBoundVars )

-------------------------------------------------------------------------------
-- Substitutions                                                             --
-------------------------------------------------------------------------------
-- | A substitution is a mapping of variable names to expressions.
newtype Subst a = Subst { substMap :: Map (S.QName a) (S.Exp a) }
 deriving Show

-------------------------------------------------------------------------------
-- Construction                                                              --
-------------------------------------------------------------------------------
-- | A substitution that does not change an expression.
identitySubst :: Subst a
identitySubst = Subst Map.empty

-- | Creates a substitution that maps variables with the given name to the
--   given expression.
singleSubst :: S.QName a -> S.Exp a -> Subst a
singleSubst = Subst .: Map.singleton

-- | Creates a substitution that maps the variables with the given names to the
--   corresponding expressions.
substFromList :: [(S.QName a, S.Exp a)] -> Subst a
substFromList = Subst . Map.fromList

-------------------------------------------------------------------------------
-- Composition                                                               --
-------------------------------------------------------------------------------
-- | Creates a new substitution that applies both given substitutions after
--   each other.
--
--   For example, the composition of the following two substitutions
--
--   > σ₂ = { x₁ ↦ e₁, …, xₙ ↦ eₙ }
--   > σ₁ = { y₁ ↦ f₁, …, yₘ ↦ fₘ }
--
--   yields the substitution.
--
--   > σ₂ ∘ σ₁ = { x₁ ↦ e₁, …, xₙ ↦ eₙ, y₁ ↦ σ₂(f₁), …, yₘ ↦ σ₂(fₘ) }
--
--   If a @xᵢ@ equals an @yⱼ@, the substitution for @yⱼ@ takes precedence.
composeSubst :: Subst a -> Subst a -> Subst a
composeSubst s2 (Subst m1) = s2 `extendSubst` Subst (fmap (applySubst s2) m1)

-- | Creates a new substitution that applies all given substitutions after
--   each other.
composeSubsts :: [Subst a] -> Subst a
composeSubsts = foldl composeSubst identitySubst

-- | Creates a new substitution that applies both substitutions without
--   composing the substitutions.
--
--   For example, the composition of the following two substitutions
--
--   > σ₂ = { x₁ ↦ e₁, …, xₙ ↦ eₙ }
--   > σ₁ = { y₁ ↦ f₁, …, yₘ ↦ fₘ }
--
--   yields the substitution.
--
--   > σ₂ ∪ σ₁ = { x₁ ↦ e₁, …, xₙ ↦ eₙ, y₁ ↦ f₁, …, yₘ ↦ fₘ }
--
--   If a @xᵢ@ equals an @yⱼ@, the substitution for @yⱼ@ takes precedence.
extendSubst :: Subst a -> Subst a -> Subst a
extendSubst (Subst m2) (Subst m1) = Subst (Map.unionWith (const id) m2 m1)

-------------------------------------------------------------------------------
-- Application                                                               --
-------------------------------------------------------------------------------
-- | Type class for applying a substitution that replaces variables by
--   expressions in values of type @node@.
class ApplySubst node where
  applySubst :: Subst a -> node a -> node a

-- | Substitutions can be applied to expressions.
instance ApplySubst S.Exp where
  applySubst subst var@(S.Var _ varName) = Map.findWithDefault var varName
    (substMap subst)
  -- If a variable is written in infix notation, it can be substituted as well.
  -- The infix operators are then written in postfix notation.
  applySubst subst (S.InfixApp appSrcSpan e1 (S.QVarOp opSrcSpan opName) e2)
    = let e1'     = applySubst subst e1
          opExpr  = S.Var opSrcSpan opName
          opExpr' = Map.findWithDefault opExpr opName (substMap subst)
          e2'     = applySubst subst e2
      in S.infixApp appSrcSpan e1' opExpr' e2'
  -- Infix constructors on the other hand cannot be substituted.
  applySubst subst (S.InfixApp srcSpan e1 op@(S.QConOp _ _) e2)
    = let e1' = applySubst subst e1
          e2' = applySubst subst e2
      in S.InfixApp srcSpan e1' op e2'
  -- The arguments of lambda abstractions and bindings of @let@ expressions
  -- must be renamed such that the substitution does not introduce name
  -- conflicts.
  applySubst subst lambdaExpr@(S.Lambda srcSpan args expr)
    = let fvs               = subst `substFreeVarSetIn` lambdaExpr
          (renaming, args') = renamePatterns fvs args
          subst'            = subst `extendSubst` renaming
          expr'             = applySubst subst' expr
      in S.Lambda srcSpan args' expr'
  applySubst subst letExpr@(S.Let srcSpan binds expr)
    = let fvs                = subst `substFreeVarSetIn` letExpr
          (renaming, binds') = renameBinds fvs binds
          subst'             = subst `extendSubst` renaming
          binds''            = applySubst subst' binds'
          expr'              = applySubst subst' expr
      in S.Let srcSpan binds'' expr'
  -- Substitute recursively.
  applySubst subst (S.App srcSpan e1 e2)
    = let e1' = applySubst subst e1
          e2' = applySubst subst e2
      in S.App srcSpan e1' e2'
  applySubst subst (S.NegApp srcSpan expr) = let expr' = applySubst subst expr
                                             in S.NegApp srcSpan expr'
  applySubst subst (S.If srcSpan e1 e2 e3)
    = let e1' = applySubst subst e1
          e2' = applySubst subst e2
          e3' = applySubst subst e3
      in S.If srcSpan e1' e2' e3'
  applySubst subst (S.Case srcSpan expr alts)
    = let expr' = applySubst subst expr
          alts' = map (applySubst subst) alts
      in S.Case srcSpan expr' alts'
  applySubst subst (S.Tuple srcSpan boxed exprs)
    = let exprs' = map (applySubst subst) exprs
      in S.Tuple srcSpan boxed exprs'
  applySubst subst (S.List srcSpan exprs)
    = let exprs' = map (applySubst subst) exprs
      in S.List srcSpan exprs'
  applySubst subst (S.Paren srcSpan expr) = let expr' = applySubst subst expr
                                            in S.Paren srcSpan expr'
  applySubst subst (S.ExpTypeSig srcSpan expr typeExpr)
    = let expr' = applySubst subst expr
      in S.ExpTypeSig srcSpan expr' typeExpr
  -- Constructors and literals remain unchanged.
  applySubst _ expr@(S.Con _ _) = expr
  applySubst _ expr@(S.Lit _ _) = expr

-- Substitutions can be applied to alternatives of @case@-expressions.
instance ApplySubst S.Alt where
  applySubst subst alt@(S.Alt srcSpan pat rhs mBinds)
    = let fvs = subst `substFreeVarSetIn` alt
          (renaming, [pat'], mBinds') = renamePatternsAndBinds fvs [pat] mBinds
          subst' = subst `extendSubst` renaming
          rhs' = applySubst subst' rhs
          mBinds'' = fmap (applySubst subst') mBinds'
      in S.Alt srcSpan pat' rhs' mBinds''

-- | Substitutions can be applied to bindings in @let@-expressions and
--   @where@-clauses
instance ApplySubst S.Binds where
  applySubst subst (S.BDecls srcSpan decls)
    = let decls' = map (applySubst subst) decls
      in S.BDecls srcSpan decls'

-- | Substitutions can be applied to declarations.
--
--   They only have an effect on function declarations.
instance ApplySubst S.Decl where
  applySubst subst (S.FunBind srcSpan matches)
    = let matches' = map (applySubst subst) matches
      in S.FunBind srcSpan matches'
  applySubst _ decl@(S.DataDecl _ _ _ _)       = decl
  applySubst _ decl@(S.OtherDecl _ _)          = decl

-- | Substitutions can be applied to matches of function declarations.
instance ApplySubst S.Match where
  applySubst subst match@(S.Match srcSpan isInfix name args rhs mBinds)
    = let fvs = subst `substFreeVarSetIn` match
          (renaming, args', mBinds') = renamePatternsAndBinds fvs args mBinds
          subst' = subst `extendSubst` renaming
          rhs' = applySubst subst' rhs
          mBinds'' = fmap (applySubst subst') mBinds'
      in S.Match srcSpan isInfix name args' rhs' mBinds''

-- | Substitutions can be applied to the right-hand sides of function
--   declarations and @case@ expressions.
instance ApplySubst S.Rhs where
  applySubst subst (S.UnGuardedRhs srcSpan expr)
    = let expr' = applySubst subst expr
      in S.UnGuardedRhs srcSpan expr'
  applySubst subst (S.GuardedRhss srcSpan grhss)
    = let grhss' = map (applySubst subst) grhss
      in S.GuardedRhss srcSpan grhss'

-- | When a substitution is applied to a guarded right-hand side, it is applied
--   to the guard and the guarded expression.
instance ApplySubst S.GuardedRhs where
  applySubst subst (S.GuardedRhs srcSpan e1 e2)
    = let e1' = applySubst subst e1
          e2' = applySubst subst e2
      in S.GuardedRhs srcSpan e1' e2'

-------------------------------------------------------------------------------
-- Free Variables of Substitutions                                           --
-------------------------------------------------------------------------------
-- | Gets the names of free variables that occur freely on right-hand sides
--   of mappings of the given substitution.
substFreeVarSet :: Subst a -> Set (S.QName a)
substFreeVarSet = Set.unions . map freeVarSet . Map.elems . substMap

-- | Gets the names of variables that occur freely in the given node and on
--   the right-hand sides of mappings of the given substitution for the free
--   variables of the node.
--
--   This function is used to determine the set of variable names that must
--   not be captured by (renamed) binders of the given node when the given
--   substitution is applied.
substFreeVarSetIn :: FreeVars node => Subst a -> node a -> Set (S.QName a)
substFreeVarSetIn subst node
  = let fvs    = freeVarSet node
        subst' = Subst (substMap subst `Map.restrictKeys` fvs)
    in substFreeVarSet subst' `Set.union` fvs

-------------------------------------------------------------------------------
-- Renaming Bound Variables                                                  --
-------------------------------------------------------------------------------
-- | Renames the given patterns such that there is no variable pattern that
--   binds one of the given names.
--
--   Returns the renamed patterns and a substitution that replaces the old
--   names by the new names including the names that have not changed.
renamePatterns :: Set (S.QName a) -> [S.Pat a] -> (Subst a, [S.Pat a])
renamePatterns = foldRenameBoundVars

-- | Renames the given local declarations such that there is no local
--   declaration that binds one of the given names.
--
--   Returns the renamed local declarations and a substitution that replaces
--   the old names by the new names including the names that have not changed.
renameBinds :: Set (S.QName a) -> S.Binds a -> (Subst a, S.Binds a)
renameBinds fvs (S.BDecls srcSpan decls)
  = S.BDecls srcSpan <$> foldRenameBoundVars fvs decls

-- | Combines an application of 'renamePatterns' with an optional application
--   of 'renameBinds'.
--
--   This function is used to rename the variables that are bound by @case@
--   expression alternatives and function matches.
renamePatternsAndBinds :: Set (S.QName a)
                       -> [S.Pat a]
                       -> Maybe (S.Binds a)
                       -> (Subst a, [S.Pat a], Maybe (S.Binds a))
renamePatternsAndBinds fvs pats mBinds
  = let (renaming, pats')    = renamePatterns fvs pats
        (renaming', mBinds') = maybe (identitySubst, Nothing)
          (fmap Just . renameBinds fvs) mBinds
    in (renaming `extendSubst` renaming', pats', mBinds')

-- | Renames the variables that are bound by the given nodes such that the
--   variables with the given names are not captured.
--
--    Returns the renamed nodes and a substitution that replaces the old
--    names by the new names including the names that have not changed.
foldRenameBoundVars
  :: (BoundVars node)
  => Set (S.QName a) -- ^ The variables that must not be captured.
  -> [node a]        -- ^ The nodes that bind variables.
  -> (Subst a, [node a])
foldRenameBoundVars _ []               = (identitySubst, [])
foldRenameBoundVars fvs (node : nodes)
  = let (renaming, node')   = renameBoundVars fvs node
        fvs'                = fvs `Set.union` substFreeVarSet renaming
        (renaming', nodes') = foldRenameBoundVars fvs' nodes
    in (renaming `extendSubst` renaming', node' : nodes')

-- | Renames the variables that are bound by the given node such that the
--   variables with the given names are not captured.
--
--   Returns the renamed node as well as a substitution that replaces the old
--   names by the new names including the names that have not changed.
renameBoundVars :: BoundVars node
                => Set (S.QName a) -- ^ The variables that must not be captured.
                -> node a          -- ^ The node that binds variables.
                -> (Subst a, node a)
renameBoundVars fvs node
  = let bvs   = boundVars node
        bvs'  = renameBoundVars' fvs bvs
        subst = substFromList (zipWith (curry (S.unQual *** S.var)) bvs bvs')
    in (subst, withBoundVars node bvs')

-- | Renames the given bound variables such that the variables with the given
--   names are not captured.
renameBoundVars'
  :: Set (S.QName a) -- ^ The variables that must not be captured.
  -> [S.Name a]      -- ^ The bound variables to rename.
  -> [S.Name a]
renameBoundVars' _ []           = []
renameBoundVars' fvs (bv : bvs)
  = let bv' = renameBoundVar fvs bv
    in bv' : renameBoundVars' (Set.insert (S.unQual bv') fvs) bvs

-- | Renames the given bound variable such that it does not capture any of the
--   variables with the given names.
--
--   If the bound variable @x@ does not capture any variable, it is not renamed.
--   Otherwise the smallest @N@ is found such that @x_N@ does not capture a
--   variable.
renameBoundVar :: Set (S.QName a) -- ^ The variables that must not be captured.
               -> S.Name a        -- ^ The bound variable to rename.
               -> S.Name a
renameBoundVar fvs bv = head (filter notCaptures (bv : suggestFreshNames bv))
 where
  -- | Tests whether the given bound variable does not capture any variable
  --   in 'fvs'.
  {- notCaptures :: S.Name a -> Bool -}
  notCaptures bv' = S.unQual bv' `Set.notMember` fvs

-------------------------------------------------------------------------------
-- Generating Fresh Names for Bound Variables                                --
-------------------------------------------------------------------------------
-- | The prefix to use for fresh variables when renaming a symbolic
--   identifier.
freshSymbolPrefix :: String
freshSymbolPrefix = "x"

-- | Generates an infinite list of fresh names to try when renaming a bound
--   variable with the given name.
--
--   The suggested names are derived from the original name by adding a @_N@
--   suffix where @N@ is a natural number. If the variable has a @_N@ suffix
--   of the form @_N@ already, the original suffix is replaced by the new one.
--
--   If the original name is a symbol, the generated names start with the
--   prefix @x@.
suggestFreshNames :: S.Name a -> [S.Name a]
suggestFreshNames name
  = [S.Ident srcSpan (prefix ++ "_" ++ show n) | n <- [0 :: Integer ..]]
 where
  -- | The source span of the original name.
  --
  --   The source span information is preserved.
  {- srcSpan :: SrcSpan a -}
  srcSpan = S.getSrcSpan name

  -- | The prefix for the fresh variable (i.e., the new identifier without
  --   the @_N@ suffix).
  --
  --   The prefix is usually the name of the original identifier of the bound
  --   variable. If the variable has a @_N@ suffix already, it is removed.
  --   If the variable is a symbol, 'freshSymbolPrefix' is used instead.
  prefix :: String
  prefix = case name of
    S.Ident _ ident -> removeSuffix ident
    S.Symbol _ _    -> freshSymbolPrefix

  -- | Removes a suffix of the form @_N@ where @N@ is an integer from a
  --   variable identifier.
  --
  --   If the identifier does not have such a suffix, it is returned unchanged.
  removeSuffix :: String -> String
  removeSuffix ident = let (ident', suffix) = breakOnEnd "_" ident
                       in if not (null ident') && all isDigit suffix
                            then dropEnd1 ident'
                            else ident
