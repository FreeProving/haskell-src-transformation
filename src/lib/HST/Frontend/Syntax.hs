{-# LANGUAGE TypeFamilies, FlexibleContexts, StandaloneDeriving #-}

-- | This module contains the AST data structure used by the pattern matching
--   compiler and some basic construction and destruction functions.
--
--   The data structure is based on the one used by @haskell-src-exts@, but
--   many constructs not supported by @haskell-src-transformations@ are
--   excluded and some others are simplified. Most notably, types for source
--   spans, literals and type expressions are not included in this data
--   structure, because they are never constructed, destructed or modified
--   during the pattern matching compilation, but are instead implemented as
--   type families. When transforming other AST data structures into this
--   structure, these type families have to be instantiated with the concrete
--   types of the former.

module HST.Frontend.Syntax where

-------------------------------------------------------------------------------
-- Type Families                                                             --
-------------------------------------------------------------------------------

-- | Type family for the type of source spans.
type family SrcSpanType a

-- | Type family for the type of @Integer@, @String@ and other literals.
type family Literal a

-- | Type family for the type of type expressions.
type family TypeExp a

-------------------------------------------------------------------------------
-- Type Family Constraints                                                   --
-------------------------------------------------------------------------------

-- | Wrapper class for the @Eq@ instance of ASTs.
--
--   Note that source span information is not compared when using @==@ on AST
--   constructs.
class (Eq (Literal a), Eq (TypeExp a)) => EqAST a

-- | Wrapper class for the @Show@ instance of ASTs.
class (Show (SrcSpanType a), Show (Literal a), Show (TypeExp a)) => ShowAST a

-------------------------------------------------------------------------------
-- Modules                                                                   --
-------------------------------------------------------------------------------

-- | A representation of a Haskell module.
--
--   Only contains a list of declarations. When transforming such a module back
--   to a complete representation of a Haskell module, it has to be enriched by
--   other information (for example the module header and unsupported
--   declarations) from the original module.
newtype Module a = Module [Decl a]
deriving instance EqAST a => Eq (Module a)
deriving instance ShowAST a => Show (Module a)

-------------------------------------------------------------------------------
-- Declarations                                                              --
-------------------------------------------------------------------------------

-- | A declaration.
--
--   Data declarations only contain the information relevant for the pattern
--   matching compiler and should not be transformed back.
--
--   The only supported kind of pattern bindings, variable patterns, are
--   represented by function bindings.
data Decl a = DataDecl (SrcSpan a) (Name a) [ConDecl a]
            | TypeSig (SrcSpan a) [Name a] (TypeExp a)
            | FunBind (SrcSpan a) [Match a]
deriving instance EqAST a => Eq (Decl a)
deriving instance ShowAST a => Show (Decl a)

-------------------------------------------------------------------------------
-- Data Type Declarations                                                    --
-------------------------------------------------------------------------------

-- | A data constructor. Does not include a source span and should not be
--   transformed back.
data ConDecl a = ConDecl (Name a) [TypeExp a]
               | InfixConDecl (TypeExp a) (Name a) (TypeExp a)
deriving instance EqAST a => Eq (ConDecl a)
deriving instance ShowAST a => Show (ConDecl a)

-------------------------------------------------------------------------------
-- Function Declarations                                                     --
-------------------------------------------------------------------------------

-- | A binding group (for example after a @where@ clause).
data Binds a = BDecls (SrcSpan a) [Decl a]
deriving instance EqAST a => Eq (Binds a)
deriving instance ShowAST a => Show (Binds a)

-- | A match belonging to a function binding declaration.
data Match a
  = Match (SrcSpan a) (Name a) [Pat a] (Rhs a) (Maybe (Binds a))
  | InfixMatch (SrcSpan a) (Pat a) (Name a) [Pat a] (Rhs a) (Maybe (Binds a))
deriving instance EqAST a => Eq (Match a)
deriving instance ShowAST a => Show (Match a)

-- | A right hand side belonging to a 'Match'.
data Rhs a = UnGuardedRhs (SrcSpan a) (Exp a)
           | GuardedRhss (SrcSpan a) [GuardedRhs a]
deriving instance EqAST a => Eq (Rhs a)
deriving instance ShowAST a => Show (Rhs a)

-- | A guarded right hand side. Only @Bool@ expressions can be used as guards.
data GuardedRhs a = GuardedRhs (SrcSpan a) (Exp a) (Exp a)
deriving instance EqAST a => Eq (GuardedRhs a)
deriving instance ShowAST a => Show (GuardedRhs a)

-------------------------------------------------------------------------------
-- Expressions                                                               --
-------------------------------------------------------------------------------

-- | Marks if a type is boxed or unboxed (most types are boxed, unboxed types
--   represent raw values).
data Boxed = Boxed
           | Unboxed
  deriving (Eq, Ord, Show)

-- | An expression.
data Exp a = Var (SrcSpan a) (QName a)
           | Con (SrcSpan a) (QName a)
           | Lit (SrcSpan a) (Literal a)
           | InfixApp (SrcSpan a) (Exp a) (QOp a) (Exp a)
           | App (SrcSpan a) (Exp a) (Exp a)
           | NegApp (SrcSpan a) (Exp a)
           | Lambda (SrcSpan a) [Pat a] (Exp a)
           | Let (SrcSpan a) (Binds a) (Exp a)
           | If (SrcSpan a) (Exp a) (Exp a) (Exp a)
           | Case (SrcSpan a) (Exp a) [Alt a]
           | Tuple (SrcSpan a) Boxed [Exp a]
           | List (SrcSpan a) [Exp a]
           | Paren (SrcSpan a) (Exp a)
           | ExpTypeSig (SrcSpan a) (Exp a) (TypeExp a)
deriving instance EqAST a => Eq (Exp a)
deriving instance ShowAST a => Show (Exp a)

-- | Creates a variable expression from a name. The additional source span
--   information is taken from the given name.
var :: Name a -> Exp a
var n@(Ident  s _) = Var s (UnQual s n)
var n@(Symbol s _) = Var s (UnQual s n)

-- | An alternative in a @case@ expression.
data Alt a = Alt (SrcSpan a) (Pat a) (Rhs a) (Maybe (Binds a))
deriving instance EqAST a => Eq (Alt a)
deriving instance ShowAST a => Show (Alt a)

-- | Creates an alternative in a @case@ expression from a pattern and an
--   expression. The additional source span information is taken from the given
--   pattern and expression.
alt :: Pat a -> Exp a -> Alt a
alt pat e = Alt (getSrcPat pat) pat (UnGuardedRhs (getSrcExp e) e) Nothing

-------------------------------------------------------------------------------
-- Patterns                                                                  --
-------------------------------------------------------------------------------

-- | A pattern.
data Pat a = PVar (SrcSpan a) (Name a)
           | PInfixApp (SrcSpan a) (Pat a) (QName a) (Pat a)
           | PApp (SrcSpan a) (QName a) [Pat a]
           | PTuple (SrcSpan a) Boxed [Pat a]
           | PParen (SrcSpan a) (Pat a)
           | PList (SrcSpan a) [Pat a]
           | PWildCard (SrcSpan a)
  deriving Eq
deriving instance ShowAST a => Show (Pat a)

-------------------------------------------------------------------------------
-- Names                                                                     --
-------------------------------------------------------------------------------

-- | A name of a Haskell module used in a qualified name.
data ModuleName a = ModuleName (SrcSpan a) String
  deriving (Eq, Ord)
deriving instance ShowAST a => Show (ModuleName a)

-- | A name possibly qualified by a 'ModuleName'.
data QName a = Qual (SrcSpan a) (ModuleName a) (Name a)
             | UnQual (SrcSpan a) (Name a)
             | Special (SrcSpan a) (SpecialCon a)
  deriving (Eq, Ord)
deriving instance ShowAST a => Show (QName a)

-- | An unqualified name.
data Name a = Ident (SrcSpan a) String
            | Symbol (SrcSpan a) String
  deriving (Eq, Ord)
deriving instance ShowAST a => Show (Name a)

-- | A possibly qualified infix operator.
data QOp a = QVarOp (SrcSpan a) (QName a)
           | QConOp (SrcSpan a) (QName a)
  deriving (Eq, Ord)
deriving instance ShowAST a => Show (QOp a)

-- | A built-in constructor with special syntax.
data SpecialCon a = UnitCon (SrcSpan a)
                  | ListCon (SrcSpan a)
                  | FunCon (SrcSpan a)
                  | TupleCon (SrcSpan a) Boxed Int
                  | Cons (SrcSpan a)
                  | UnboxedSingleCon (SrcSpan a)
                  | ExprHole (SrcSpan a)
  deriving (Eq, Ord)
deriving instance ShowAST a => Show (SpecialCon a)

-------------------------------------------------------------------------------
-- Source Spans                                                              --
-------------------------------------------------------------------------------

-- | A wrapper for source span information with the option to not specify a
--   source span.
data SrcSpan a = SrcSpan (SrcSpanType a)
               | NoSrcSpan
deriving instance ShowAST a => Show (SrcSpan a)

-- | Custom equality instance for 'SrcSpan' which always returns @True@.
instance Eq (SrcSpan a) where
  _ == _ = True

-- | Custom order for 'SrcSpan' which treats all source spans as equal.
instance Ord (SrcSpan a) where
  _ `compare` _ = EQ

-- | Returns the top-level source span information of an expression.
getSrcExp :: Exp a -> SrcSpan a
getSrcExp (Var src _         ) = src
getSrcExp (Con src _         ) = src
getSrcExp (Lit src _         ) = src
getSrcExp (InfixApp src _ _ _) = src
getSrcExp (App src _ _       ) = src
getSrcExp (NegApp src _      ) = src
getSrcExp (Lambda src _ _    ) = src
getSrcExp (Let    src _ _    ) = src
getSrcExp (If src _ _ _      ) = src
getSrcExp (Case  src _ _     ) = src
getSrcExp (Tuple src _ _     ) = src
getSrcExp (List  src _       ) = src
getSrcExp (Paren src _       ) = src
getSrcExp (ExpTypeSig src _ _) = src

-- | Returns the top-level source span information of a pattern.
getSrcPat :: Pat a -> SrcSpan a
getSrcPat (PVar src _         ) = src
getSrcPat (PInfixApp src _ _ _) = src
getSrcPat (PApp   src _ _     ) = src
getSrcPat (PTuple src _ _     ) = src
getSrcPat (PParen src _       ) = src
getSrcPat (PList  src _       ) = src
getSrcPat (PWildCard src      ) = src
