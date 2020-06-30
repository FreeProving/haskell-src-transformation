-- | This module contains definitions for substitutions of variables.

module HST.Environment.Renaming where

import           HST.Environment.FreshVars      ( PM
                                                , freshVar
                                                )
import qualified Language.Haskell.Exts.Syntax  as HSE

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
type TSubst l
  =  HSE.Exp l -- VarExp
  -> HSE.Exp l -- Constructor application expression

-- Smart constructor for 'TSubst' that creates a substitution that replaces
-- the given variable expression (first argument) with the second expression.
tSubst :: Eq l => HSE.Exp l -> HSE.Exp l -> TSubst l
tSubst vE tE = \v -> if v == vE then tE else v

-- | Type class for AST nodes of type @c@ a 'TSubst' can be applied to.
class TermSubst c where
  -- | Applies the given substitution to the given node.
  substitute :: TSubst l -> c l -> c l

-- | 'TermSubst' instance for expressions.
instance TermSubst HSE.Exp where
  substitute s expr = case expr of
    HSE.Var l qname   -> s (HSE.Var l qname)
    HSE.Con l qname   -> HSE.Con l qname
    HSE.Lit l literal -> HSE.Lit l literal
    HSE.InfixApp l e1 qop e2 ->
      HSE.InfixApp l (substitute s e1) qop (substitute s e2)
    HSE.App    l e1 e2 -> HSE.App l (substitute s e1) (substitute s e2)
    HSE.Lambda l ps e  -> HSE.Lambda l ps (substitute s e)
    HSE.Let    l b  e  -> HSE.Let l b $ substitute s e
    HSE.If l e1 e2 e3 ->
      HSE.If l (substitute s e1) (substitute s e2) (substitute s e3)
    HSE.Case  l e   as   -> HSE.Case l e (map (substitute s) as)   -- TODO no subst for debugging (substitute s e)
    HSE.Tuple l bxd es   -> HSE.Tuple l bxd (map (substitute s) es)
    HSE.List  l es       -> HSE.List l (map (substitute s) es)
    HSE.Paren l e        -> HSE.Paren l (substitute s e)
    HSE.ListComp   _ _ _ -> error "TermSubst: List comp is not supported"
    HSE.ExpTypeSig l e t -> HSE.ExpTypeSig l (substitute s e) t
    _                    -> error "TermSubst: Exp caused an error"

-- | 'TermSubst' instance for @case@ expression alternatives.
--
--   Applies the substitution to the right-hand side.
instance TermSubst HSE.Alt where
  substitute s (HSE.Alt l p rhs mb) = HSE.Alt l p (substitute s rhs) mb

-- | 'TermSubst' instance for right-hand sides.
--
--   Applies the substitution to the expression on the right-hand side.
--   There must be no guards.
instance TermSubst HSE.Rhs where
  substitute s rhs = case rhs of
    HSE.UnGuardedRhs l e -> HSE.UnGuardedRhs l $ substitute s e
    HSE.GuardedRhss  _ _ -> error "TermSubst: GuardedRhss not supported"

-- | Type class for AST nodes of type @c@ a 'Subst' can be applied to.
class Rename c where
  -- | Renames all variables that occur in the given AST node.
  rename :: Subst -> c l -> c l

-- | 'Rename' instance for expressions.
instance Rename HSE.Exp where
    -- rename :: Subst -> Exp l -> Exp l
  rename s expr = case expr of
    HSE.Var l qname          -> HSE.Var l (rename s qname)
    HSE.Con l qname          -> HSE.Con l qname
    HSE.Lit l literal        -> HSE.Lit l literal
    HSE.InfixApp l e1 qop e2 -> HSE.InfixApp l (rename s e1) qop (rename s e2)
    HSE.App    l e1 e2       -> HSE.App l (rename s e1) (rename s e2)
    HSE.Lambda l ps e        -> HSE.Lambda l (map (rename s) ps) (rename s e)
    HSE.Let    l b  e        -> HSE.Let l b $ rename s e
    HSE.If l e1 e2 e3 -> HSE.If l (rename s e1) (rename s e2) (rename s e3)
    HSE.Case  l e   as       -> HSE.Case l (rename s e) (map (rename s) as)
    HSE.Tuple l bxd es       -> HSE.Tuple l bxd (map (rename s) es)
    HSE.List  l es           -> HSE.List l (map (rename s) es)
    HSE.Paren l e            -> HSE.Paren l (rename s e)
    HSE.ListComp   _ _ _     -> error "Rename: List comp is not supported"
    HSE.ExpTypeSig l e t     -> HSE.ExpTypeSig l (rename s e) t
    _                        -> error "Rename: Exp caused an error"

-- | 'Rename' instance for optionally qualified variable names.
instance Rename HSE.QName where
  rename s qname = case qname of
    -- TODO qualified variables should not be renamed.
    HSE.Qual l mname name -> HSE.Qual l mname (rename s name)
    HSE.UnQual  l name    -> HSE.UnQual l (rename s name)
    HSE.Special l special -> HSE.Special l special

-- | 'Rename' instance for variable names.
instance Rename HSE.Name where
  rename s name = case name of
    HSE.Ident  l str -> HSE.Ident l $ s str
    HSE.Symbol l str -> HSE.Ident l $ s str

-- | 'Rename' instance for @case@ expression alternatives.
--
--   Variables are renamed on the right-hand side and in variable patterns.
--   TODO is it actually wanted to replace variable binders?
instance Rename HSE.Alt where
  rename s (HSE.Alt l p rhs mB) = HSE.Alt l (rename s p) (rename s rhs) mB

-- | 'Rename' instance for patterns.
instance Rename HSE.Pat where
  rename s pat = case pat of
    HSE.PVar l name -> HSE.PVar l $ rename s name
    HSE.PInfixApp l p1 qname p2 ->
      HSE.PInfixApp l (rename s p1) (rename s qname) (rename s p2)
    HSE.PApp   l qname ps -> HSE.PApp l (rename s qname) (map (rename s) ps)
    HSE.PTuple l boxed ps -> HSE.PTuple l boxed (map (rename s) ps)
    HSE.PList  l ps       -> HSE.PList l (map (rename s) ps)
    HSE.PParen l p        -> HSE.PParen l $ rename s p
    HSE.PWildCard l       -> HSE.PWildCard l
    _                     -> error "Rename: Pat caused an error"

-- | 'Rename' instance for right-hand sides.
--
--   Applies the substitution to the expression on the right-hand side.
--   There must be no guards.
instance Rename HSE.Rhs where
  rename s rhs = case rhs of
    HSE.UnGuardedRhs l e -> HSE.UnGuardedRhs l $ rename s e
    HSE.GuardedRhss  _ _ -> error "Rename: GuardedRhss not supported"

-- | Creates a new fresh variable pattern for the given variable pattern.
renamePVar :: HSE.Pat l -> PM (HSE.Pat l)
renamePVar (HSE.PVar l name) = do
  nname <- newName name
  return (HSE.PVar l nname)
renamePVar _ = error "no variable in renamePVar"

-- | Generates a fresh variable name with an ID from the state.
--
--   The given argument must be an identifier. Only the annotation of the
--   identifier is preserved.
newName :: HSE.Name l -> PM (HSE.Name l)
newName (HSE.Ident l _) = do
  var <- freshVar
  return (HSE.Ident l ('a' : show var))
newName _ = error "no Ident in newName"

-- | Generates a fresh variable name with an ID from the state.
genVar :: PM (HSE.Name ())
genVar = do
  x <- freshVar
  return (HSE.Ident () ('a' : show x))
