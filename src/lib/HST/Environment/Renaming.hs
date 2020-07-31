-- | This module contains definitions for substitutions of variables.

module HST.Environment.Renaming where

import qualified HST.Frontend.Syntax           as S

-- | A substitution (or "renaming") is a mapping of variable names to variable
--   names.
type Subst = String -> String

-- Smart constructor for 'Subst' that creates a substitution that renames
-- variables with the first name to variables with the second name.
subst :: String -> String -> Subst
subst s1 s2 = \s -> if s == s1 then s2 else s

-- | Combines two substitutions.
--
--   Applying the substitution returned by @compose s1 s2@ is the same as
--   applying first @s2@ and then @s1@.
compose :: Subst -> Subst -> Subst
compose = (.)

-- | Type for substations extended from variable names to expressions.
type TSubst a
  =  S.Exp a -- VarExp
  -> S.Exp a -- Constructor application expression

-- Smart constructor for 'TSubst' that creates a substitution that replaces
-- the given variable expression (first argument) with the second expression.
tSubst :: S.EqAST a => S.Exp a -> S.Exp a -> TSubst a
tSubst vE tE = \v -> if v == vE then tE else v

-- | Type class for AST nodes of type @c@ a 'TSubst' can be applied to.
class TermSubst c where
  -- | Applies the given substitution to the given node.
  substitute :: TSubst a -> c a -> c a

-- | 'TermSubst' instance for expressions.
instance TermSubst S.Exp where
  substitute s (S.Var l qname  ) = s (S.Var l qname)
  substitute _ (S.Con l qname  ) = S.Con l qname
  substitute _ (S.Lit l literal) = S.Lit l literal
  substitute s (S.InfixApp l e1 qop e2) =
    S.InfixApp l (substitute s e1) qop (substitute s e2)
  substitute s (S.NegApp l e    ) = S.NegApp l (substitute s e)
  substitute s (S.App    l e1 e2) = S.App l (substitute s e1) (substitute s e2)
  substitute s (S.Lambda l ps e ) = S.Lambda l ps (substitute s e)
  substitute s (S.Let    l b  e ) = S.Let l b $ substitute s e
  substitute s (S.If l e1 e2 e3) =
    S.If l (substitute s e1) (substitute s e2) (substitute s e3)
  substitute s (S.Case l e as) =
    S.Case l (substitute s e) (map (substitute s) as)
  substitute s (S.Tuple l bxd es  ) = S.Tuple l bxd (map (substitute s) es)
  substitute s (S.List  l es      ) = S.List l (map (substitute s) es)
  substitute s (S.Paren l e       ) = S.Paren l (substitute s e)
  substitute s (S.ExpTypeSig l e t) = S.ExpTypeSig l (substitute s e) t

-- | 'TermSubst' instance for @case@ expression alternatives.
--
--   Applies the substitution to the right-hand side.
instance TermSubst S.Alt where
  substitute s (S.Alt l p rhs mb) = S.Alt l p (substitute s rhs) mb

-- | 'TermSubst' instance for right-hand sides.
--
--   Applies the substitution to the expression on the right-hand side.
--   There must be no guards.
instance TermSubst S.Rhs where
  substitute s (S.UnGuardedRhs l e) = S.UnGuardedRhs l (substitute s e)
  substitute s (S.GuardedRhss l grhs) =
    S.GuardedRhss l (map (substitute s) grhs)

instance TermSubst S.GuardedRhs where
  substitute s (S.GuardedRhs l e1 e2) =
    S.GuardedRhs l (substitute s e1) (substitute s e2)

-- | Type class for AST nodes of type @c@ a 'Subst' can be applied to.
class Rename c where
  -- | Renames all variables that occur in the given AST node.
  rename :: Subst -> c -> c

-- | 'Rename' instance for expressions.
instance Rename (S.Exp a) where
  rename s (S.Var l qname  ) = S.Var l (rename s qname)
  rename _ (S.Con l qname  ) = S.Con l qname
  rename _ (S.Lit l literal) = S.Lit l literal
  rename s (S.InfixApp l e1 qop e2) =
    S.InfixApp l (rename s e1) qop (rename s e2)
  rename s (S.NegApp l e      ) = S.NegApp l (rename s e)
  rename s (S.App    l e1 e2  ) = S.App l (rename s e1) (rename s e2)
  rename s (S.Lambda l ps e   ) = S.Lambda l (map (rename s) ps) (rename s e)
  rename s (S.Let    l b  e   ) = S.Let l b $ rename s e
  rename s (S.If l e1 e2 e3) = S.If l (rename s e1) (rename s e2) (rename s e3)
  rename s (S.Case  l e   as  ) = S.Case l (rename s e) (map (rename s) as)
  rename s (S.Tuple l bxd es  ) = S.Tuple l bxd (map (rename s) es)
  rename s (S.List  l es      ) = S.List l (map (rename s) es)
  rename s (S.Paren l e       ) = S.Paren l (rename s e)
  rename s (S.ExpTypeSig l e t) = S.ExpTypeSig l (rename s e) t

-- | 'Rename' instance for optionally qualified variable names.
instance Rename (S.QName a) where
    -- TODO qualified variables should not be renamed.
  rename s (S.Qual l mname name) = S.Qual l mname (rename s name)
  rename s (S.UnQual  l name   ) = S.UnQual l (rename s name)
  rename _ (S.Special l special) = S.Special l special

-- | 'Rename' instance for variable names.
instance Rename (S.Name a) where
  rename s (S.Ident  l str) = S.Ident l $ s str
  rename s (S.Symbol l str) = S.Ident l $ s str

-- | 'Rename' instance for @case@ expression alternatives.
--
--   Variables are renamed on the right-hand side and in variable patterns.
--   TODO is it actually wanted to replace variable binders?
instance Rename (S.Alt s) where
  rename s (S.Alt l p rhs mB) = S.Alt l (rename s p) (rename s rhs) mB

-- | 'Rename' instance for patterns.
instance Rename (S.Pat a) where
  rename s (S.PVar l name) = S.PVar l (rename s name)
  rename s (S.PInfixApp l p1 qname p2) =
    S.PInfixApp l (rename s p1) (rename s qname) (rename s p2)
  rename s (S.PApp   l qname ps) = S.PApp l (rename s qname) (map (rename s) ps)
  rename s (S.PTuple l boxed ps) = S.PTuple l boxed (map (rename s) ps)
  rename s (S.PList  l ps      ) = S.PList l (map (rename s) ps)
  rename s (S.PParen l p       ) = S.PParen l (rename s p)
  rename _ (S.PWildCard l      ) = S.PWildCard l

-- | 'Rename' instance for right-hand sides.
--
--   Applies the substitution to all guards and the expressions of the
--   right-hand sides.
instance Rename (S.Rhs s) where
  rename s (S.UnGuardedRhs l e   ) = S.UnGuardedRhs l (rename s e)
  rename s (S.GuardedRhss  l rhss) = S.GuardedRhss l (map (rename s) rhss)

-- | 'Rename' instance for right-hand sides with guards.
instance Rename (S.GuardedRhs s) where
  rename s (S.GuardedRhs l e1 e2) = S.GuardedRhs l (rename s e1) (rename s e2)
