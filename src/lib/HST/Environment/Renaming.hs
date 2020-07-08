-- | This module contains definitions for substitutions of variables.

module HST.Environment.Renaming where

import           HST.Environment.FreshVars      ( PM
                                                , freshVar
                                                )
import qualified HST.Frontend.Syntax           as S
import qualified HST.Frontend.Build            as B

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
type TSubst s l t
  =  S.Exp s l t -- VarExp
  -> S.Exp s l t -- Constructor application expression

-- Smart constructor for 'TSubst' that creates a substitution that replaces
-- the given variable expression (first argument) with the second expression.
tSubst :: (Eq l, Eq t) => S.Exp s l t -> S.Exp s l t -> TSubst s l t
tSubst vE tE = \v -> if v == vE then tE else v

-- | Type class for AST nodes of type @c@ a 'TSubst' can be applied to.
class TermSubst c where
  -- | Applies the given substitution to the given node.
  substitute :: TSubst s l t -> c s l t -> c s l t

-- | 'TermSubst' instance for expressions.
instance TermSubst S.Exp where
  substitute s expr = case expr of
    S.Var l qname   -> s (S.Var l qname)
    S.Con l qname   -> S.Con l qname
    S.Lit l literal -> S.Lit l literal
    S.InfixApp l e1 qop e2 ->
      S.InfixApp l (substitute s e1) qop (substitute s e2)
    S.App    l e1 e2 -> S.App l (substitute s e1) (substitute s e2)
    S.Lambda l ps e  -> S.Lambda l ps (substitute s e)
    S.Let    l b  e  -> S.Let l b $ substitute s e
    S.If l e1 e2 e3 ->
      S.If l (substitute s e1) (substitute s e2) (substitute s e3)
    S.Case  l e   as   -> S.Case l e (map (substitute s) as)   -- no subst for debugging (substitute s e)
    S.Tuple l bxd es   -> S.Tuple l bxd (map (substitute s) es)
    S.List  l es       -> S.List l (map (substitute s) es)
    S.Paren l e        -> S.Paren l (substitute s e)
    S.ExpTypeSig l e t -> S.ExpTypeSig l (substitute s e) t
    _                  -> error "TermSubst: Exp caused an error"

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
  substitute s rhs = case rhs of
    S.UnGuardedRhs l e -> S.UnGuardedRhs l $ substitute s e
    S.GuardedRhss  _ _ -> error "TermSubst: GuardedRhss not supported"

-- | Type class for AST nodes of type @c@ a 'Subst' can be applied to.
class Rename c where
  -- | Renames all variables that occur in the given AST node.
  rename :: Subst -> c -> c

-- | 'Rename' instance for expressions.
instance Rename (S.Exp s l t) where
    -- rename :: Subst -> Exp l -> Exp l
  rename s expr = case expr of
    S.Var l qname          -> S.Var l (rename s qname)
    S.Con l qname          -> S.Con l qname
    S.Lit l literal        -> S.Lit l literal
    S.InfixApp l e1 qop e2 -> S.InfixApp l (rename s e1) qop (rename s e2)
    S.App    l e1 e2       -> S.App l (rename s e1) (rename s e2)
    S.Lambda l ps e        -> S.Lambda l (map (rename s) ps) (rename s e)
    S.Let    l b  e        -> S.Let l b $ rename s e
    S.If l e1 e2 e3        -> S.If l (rename s e1) (rename s e2) (rename s e3)
    S.Case  l e   as       -> S.Case l (rename s e) (map (rename s) as)
    S.Tuple l bxd es       -> S.Tuple l bxd (map (rename s) es)
    S.List  l es           -> S.List l (map (rename s) es)
    S.Paren l e            -> S.Paren l (rename s e)
    S.ExpTypeSig l e t     -> S.ExpTypeSig l (rename s e) t
    _                      -> error "Rename: Exp caused an error"

-- | 'Rename' instance for optionally qualified variable names.
instance Rename (S.QName s) where
  rename s qname = case qname of
    -- TODO qualified variables should not be renamed.
    S.Qual l mname name -> S.Qual l mname (rename s name)
    S.UnQual  l name    -> S.UnQual l (rename s name)
    S.Special l special -> S.Special l special

-- | 'Rename' instance for variable names.
instance Rename (S.Name s) where
  rename s name = case name of
    S.Ident  l str -> S.Ident l $ s str
    S.Symbol l str -> S.Ident l $ s str

-- | 'Rename' instance for @case@ expression alternatives.
--
--   Variables are renamed on the right-hand side and in variable patterns.
--   TODO is it actually wanted to replace variable binders?
instance Rename (S.Alt s l t) where
  rename s (S.Alt l p rhs mB) = S.Alt l (rename s p) (rename s rhs) mB

-- | 'Rename' instance for patterns.
instance Rename (S.Pat s l) where
  rename s pat = case pat of
    S.PVar l name -> S.PVar l $ rename s name
    S.PInfixApp l p1 qname p2 ->
      S.PInfixApp l (rename s p1) (rename s qname) (rename s p2)
    S.PApp   l qname ps -> S.PApp l (rename s qname) (map (rename s) ps)
    S.PTuple l boxed ps -> S.PTuple l boxed (map (rename s) ps)
    S.PList  l ps       -> S.PList l (map (rename s) ps)
    S.PParen l p        -> S.PParen l $ rename s p
    S.PWildCard l       -> S.PWildCard l

-- | 'Rename' instance for right-hand sides.
--
--   Applies the substitution to the expression on the right-hand side.
--   There must be no guards.
instance Rename (S.Rhs s l t) where
  rename s rhs = case rhs of
    S.UnGuardedRhs l e -> S.UnGuardedRhs l $ rename s e
    S.GuardedRhss  _ _ -> error "Rename: GuardedRhss not supported"

-- | Creates a new fresh variable pattern for the given variable pattern.
renamePVar :: S.Pat s l -> PM s l t (S.Pat s l)
renamePVar (S.PVar l name) = do
  nname <- newName name
  return (S.PVar l nname)
renamePVar _ = error "no variable in renamePVar"

-- | Generates a fresh variable name with an ID from the state.
--
--   The given argument must be an identifier. Only the annotation of the
--   identifier is preserved.
newName :: S.Name s -> PM s l t (S.Name s)
newName (S.Ident l _) = do
  var <- freshVar
  return (S.Ident l ('a' : show var))
newName _ = error "no Ident in newName"

-- | Generates a fresh variable name with an ID from the state.
genVar :: PM s l t (S.Name s)
genVar = do
  x <- freshVar
  return (S.Ident B.noSrc ('a' : show x))
