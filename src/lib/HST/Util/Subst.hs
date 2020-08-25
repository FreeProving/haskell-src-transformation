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
    -- * Application
  , ApplySubst(..)
  ) where

import           Data.Composition    ( (.:) )
import           Data.Map.Strict     ( Map )
import qualified Data.Map.Strict     as Map

import qualified HST.Frontend.Syntax as S

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
composeSubst :: Subst a -> Subst a -> Subst a
composeSubst s2@(Subst m2) (Subst m1)
  = let m1' = fmap (applySubst s2) m1
        m2' = Map.filterWithKey (\v _ -> v `Map.notMember` m1) m2
    in Subst (m2' `Map.union` m1')

-- | Creates a new substitution that applies all given substitutions after
--   each other.
composeSubsts :: [Subst a] -> Subst a
composeSubsts = foldl composeSubst identitySubst

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
  -- If a variable is written in infix notation, it can be substituted as
  -- well. The infix operators are then written in postfix notation.
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
  applySubst subst (S.Lambda srcSpan args expr)
    = let (subst', args') = renamePatterns subst args
          expr'           = applySubst subst' expr
      in S.Lambda srcSpan args' expr'
  applySubst subst (S.Let srcSpan binds expr)
    = let (subst', binds') = renameBinds subst binds
          binds''          = applySubst subst' binds'
          expr'            = applySubst subst' expr
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
  applySubst subst (S.Alt srcSpan pat rhs mBinds)
    = let (subst', [pat'], mBinds') = renamePatternsAndBinds subst [pat] mBinds
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
--   They only have can effect on function declarations.
instance ApplySubst S.Decl where
  applySubst subst (S.FunBind srcSpan matches)
    = let matches' = map (applySubst subst) matches
      in S.FunBind srcSpan matches'
  applySubst _ decl@(S.DataDecl _ _ _ _)       = decl
  applySubst _ decl@(S.OtherDecl _ _)          = decl

-- | Substitutions can be applied to matches of function declarations.
instance ApplySubst S.Match where
  applySubst subst (S.Match srcSpan name args rhs mBinds)
    = let (subst', args', mBinds') = renamePatternsAndBinds subst args mBinds
          rhs'                     = applySubst subst' rhs
          mBinds''                 = fmap (applySubst subst') mBinds'
      in S.Match srcSpan name args' rhs' mBinds''
  applySubst subst (S.InfixMatch srcSpan arg name args rhs mBinds)
    = let (subst', arg' : args', mBinds') = renamePatternsAndBinds subst
            (arg : args) mBinds
          rhs' = applySubst subst' rhs
          mBinds'' = fmap (applySubst subst') mBinds'
      in S.InfixMatch srcSpan arg' name args' rhs' mBinds''

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
-- Renaming arguments and binders                                            --
-------------------------------------------------------------------------------
-- | TODO
renamePatterns :: Subst a -> [S.Pat a] -> (Subst a, [S.Pat a])
renamePatterns subst pats = (subst, pats)

-- | TODO
renameBinds :: Subst a -> S.Binds a -> (Subst a, S.Binds a)
renameBinds subst binds = (subst, binds)

-- | TODO
renamePatternsAndBinds :: Subst a
                       -> [S.Pat a]
                       -> Maybe (S.Binds a)
                       -> (Subst a, [S.Pat a], Maybe (S.Binds a))
renamePatternsAndBinds subst pats mBinds
  = let (subst', pats')    = renamePatterns subst pats
        (subst'', mBinds') = maybe (subst', mBinds)
          (fmap Just . renameBinds subst') mBinds
    in (subst'', pats', mBinds')
