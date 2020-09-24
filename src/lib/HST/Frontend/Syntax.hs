{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

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

-- | Type family for a type that stores unsupported fields of a module head.
type family OriginalModuleHead a

-- | Type family for the type of unsupported declarations.
type family OriginalDecl a

-------------------------------------------------------------------------------
-- Type Family Constraints                                                   --
-------------------------------------------------------------------------------
-- | Wrapper class for the @Eq@ instance of ASTs.
--
--   Note that source span information is not compared when using @==@ on AST
--   constructs.
class ( Eq (Literal a)
      , Eq (TypeExp a)
      , Eq (OriginalModuleHead a)
      , Eq (OriginalDecl a)
      ) => EqAST a

-- | Wrapper class for the @Show@ instance of ASTs.
class ( Show (SrcSpanType a)
      , Show (Literal a)
      , Show (TypeExp a)
      , Show (OriginalModuleHead a)
      , Show (OriginalDecl a)
      ) => ShowAST a

-------------------------------------------------------------------------------
-- Modules                                                                   --
-------------------------------------------------------------------------------
-- | A representation of a Haskell module.
--
--   The pattern matching compiler only needs to know the declarations, the
--   name and the import declarations of the module. All other information is
--   stored in the 'OriginalModuleHead'.
data Module a = Module (SrcSpan a)
                       (OriginalModuleHead a)
                       (Maybe (ModuleName a))
                       [ImportDecl a]
                       [Decl a]

deriving instance EqAST a => Eq (Module a)

deriving instance ShowAST a => Show (Module a)

-- | Gets the source span information of a module.
instance HasSrcSpan Module where
  getSrcSpan (Module srcSpan _ _ _ _) = srcSpan

-------------------------------------------------------------------------------
-- Declarations                                                              --
-------------------------------------------------------------------------------
-- | A declaration.
--
--   The only supported kind of pattern bindings, variable patterns, are
--   represented by function bindings.
data Decl a
  = DataDecl (SrcSpan a) (OriginalDecl a) (Name a) [ConDecl a]
    -- ^ A @data@ or @newtype@ declaration.
    --
    --   The 'OriginalDecl' is the original declaration the data type
    --   declaration has been generated from. This field is needed to
    --   convert the AST back to the original AST since data type and
    --   constructor declarations don't contain sufficient information
    --   to reconstruct the original declaration.
  | FunBind (SrcSpan a) [Match a]
    -- ^ A function declaration.
    --
    --   We do not need to store the 'OriginalDecl' of the function
    --   declaration since the AST provides all information that is
    --   required to reconstruct the original declaration.
  | OtherDecl (SrcSpan a) (OriginalDecl a)
-- ^ An unsupported declaration.
--
--   This constructor is needed to skip declarations that cannot
--   be transformed by the pattern matching compiler.

deriving instance EqAST a => Eq (Decl a)

deriving instance ShowAST a => Show (Decl a)

-- | Gets the source span information of a declaration.
instance HasSrcSpan Decl where
  getSrcSpan (DataDecl srcSpan _ _ _) = srcSpan
  getSrcSpan (FunBind srcSpan _)      = srcSpan
  getSrcSpan (OtherDecl srcSpan _)    = srcSpan

-- | An import declaration supporting regular imports, qualified imports and
--   alias names.
--
--   Import declarations should not be converted back. The original import
--   declarations should be part of the 'OriginalModuleHead' of the 'Module'.
data ImportDecl a = ImportDecl
  { importSrcSpan :: SrcSpan a
  , importModule  :: ModuleName a
  , importIsQual  :: Bool
  , importAsName  :: Maybe (ModuleName a)
  }
 deriving Eq

deriving instance ShowAST a => Show (ImportDecl a)

-- | Gets the source span information of an import declaration.
instance HasSrcSpan ImportDecl where
  getSrcSpan = importSrcSpan

-------------------------------------------------------------------------------
-- Data Type Declarations                                                    --
-------------------------------------------------------------------------------
-- | A data constructor.
--
--   Data constructors should not be converted back. The original constructor
--   declaration should be part of the 'OriginalDecl' of a 'DataDecl'.
data ConDecl a = ConDecl { conDeclSrcSpan :: SrcSpan a
                         , conDeclName    :: Name a
                         , conDeclArity   :: Int
                         , conDeclIsInfix :: Bool
                         }
 deriving Eq

deriving instance ShowAST a => Show (ConDecl a)

-- | Gets the source span information of a constructor declaration.
instance HasSrcSpan ConDecl where
  getSrcSpan = conDeclSrcSpan

-------------------------------------------------------------------------------
-- Function Declarations                                                     --
-------------------------------------------------------------------------------
-- | A binding group (for example after a @where@ clause).
data Binds a = BDecls (SrcSpan a) [Decl a]

deriving instance EqAST a => Eq (Binds a)

deriving instance ShowAST a => Show (Binds a)

-- | Gets the source span information of a binding group.
instance HasSrcSpan Binds where
  getSrcSpan (BDecls srcSpan _) = srcSpan

-- | A match belonging to a function binding declaration.
data Match a = Match
  { matchSrcSpan :: SrcSpan a
  , matchIsInfix :: Bool
  , matchName    :: Name a
  , matchPats    :: [Pat a]
  , matchRhs     :: Rhs a
  , matchBinds   :: Maybe (Binds a)
  }

deriving instance EqAST a => Eq (Match a)

deriving instance ShowAST a => Show (Match a)

-- | Gets the source span information of a match of a function declaration.
instance HasSrcSpan Match where
  getSrcSpan = matchSrcSpan

-- | A right hand side belonging to a 'Match'.
data Rhs a
  = UnGuardedRhs (SrcSpan a) (Exp a)
  | GuardedRhss (SrcSpan a) [GuardedRhs a]

deriving instance EqAST a => Eq (Rhs a)

deriving instance ShowAST a => Show (Rhs a)

-- | Gets the source span information of a right-hand side.
instance HasSrcSpan Rhs where
  getSrcSpan (UnGuardedRhs srcSpan _) = srcSpan
  getSrcSpan (GuardedRhss srcSpan _)  = srcSpan

-- | A guarded right hand side. Only @Bool@ expressions can be used as guards.
data GuardedRhs a = GuardedRhs (SrcSpan a) (Exp a) (Exp a)

deriving instance EqAST a => Eq (GuardedRhs a)

deriving instance ShowAST a => Show (GuardedRhs a)

-- | Gets the source span information of a right-hand side with guard.
instance HasSrcSpan GuardedRhs where
  getSrcSpan (GuardedRhs srcSpan _ _) = srcSpan

-------------------------------------------------------------------------------
-- Expressions                                                               --
-------------------------------------------------------------------------------
-- | Marks if a type is boxed or unboxed (most types are boxed, unboxed types
--   represent raw values).
data Boxed = Boxed | Unboxed
 deriving ( Eq, Ord, Show )

-- | An expression.
data Exp a
  = Var (SrcSpan a) (QName a)
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

-- | Returns the top-level source span information of an expression.
instance HasSrcSpan Exp where
  getSrcSpan (Var srcSpan _)          = srcSpan
  getSrcSpan (Con srcSpan _)          = srcSpan
  getSrcSpan (Lit srcSpan _)          = srcSpan
  getSrcSpan (InfixApp srcSpan _ _ _) = srcSpan
  getSrcSpan (App srcSpan _ _)        = srcSpan
  getSrcSpan (NegApp srcSpan _)       = srcSpan
  getSrcSpan (Lambda srcSpan _ _)     = srcSpan
  getSrcSpan (Let srcSpan _ _)        = srcSpan
  getSrcSpan (If srcSpan _ _ _)       = srcSpan
  getSrcSpan (Case srcSpan _ _)       = srcSpan
  getSrcSpan (Tuple srcSpan _ _)      = srcSpan
  getSrcSpan (List srcSpan _)         = srcSpan
  getSrcSpan (Paren srcSpan _)        = srcSpan
  getSrcSpan (ExpTypeSig srcSpan _ _) = srcSpan

-- | Creates a variable expression from a name.
--
--   The additional source span information is taken from the given name.
var :: (HasSrcSpan name, QNameLike name) => name a -> Exp a
var name = Var (getSrcSpan name) (toQName name)

-- | Creates a constructor expression from a name.
--
--   The additional source span information is taken from the given name.
con :: (HasSrcSpan name, QNameLike name) => name a -> Exp a
con name = Con (getSrcSpan name) (toQName name)

-- | Creates an application expression.
--
--   The given source span is inserted into every generated application node.
app :: SrcSpan a -- ^ The source span to insert.
    -> Exp a     -- ^ The expression to apply.
    -> [Exp a]   -- ^ The arguments to apply the expression to.
    -> Exp a
app srcSpan = foldl (App srcSpan)

-- | Creates an application expression in infix notation if possible.
--
--   If the second given expression is a variable or constructor, an infix
--   application is created. Otherwise, a regular application expression is
--   created instead.
infixApp :: SrcSpan a -- ^ The source span to insert.
         -> Exp a     -- ^ The left operand.
         -> Exp a     -- ^ The expression to apply.
         -> Exp a     -- ^ The right operand.
         -> Exp a
infixApp appSrcSpan e1 opExpr e2 = case exprToQOp opExpr of
  Nothing -> app appSrcSpan opExpr [e1, e2]
  Just op -> InfixApp appSrcSpan e1 op e2
 where
  exprToQOp :: Exp a -> Maybe (QOp a)
  exprToQOp (Var opSrcSpan opName) = Just (QVarOp opSrcSpan opName)
  exprToQOp (Con opSrcSpan opName) = Just (QVarOp opSrcSpan opName)
  exprToQOp _ = Nothing

-- | An alternative in a @case@ expression.
data Alt a = Alt (SrcSpan a) (Pat a) (Rhs a) (Maybe (Binds a))

deriving instance EqAST a => Eq (Alt a)

deriving instance ShowAST a => Show (Alt a)

-- | Gets the source span information of an alternative.
instance HasSrcSpan Alt where
  getSrcSpan (Alt srcSpan _ _ _) = srcSpan

-- | Creates an alternative in a @case@ expression from a pattern and an
--   expression. The additional source span information is taken from the given
--   pattern and expression.
alt :: Pat a -> Exp a -> Alt a
alt pat e = Alt (getSrcSpan pat) pat (UnGuardedRhs (getSrcSpan e) e) Nothing

-------------------------------------------------------------------------------
-- Patterns                                                                  --
-------------------------------------------------------------------------------
-- | A pattern.
data Pat a
  = PVar (SrcSpan a) (Name a)
  | PInfixApp (SrcSpan a) (Pat a) (QName a) (Pat a)
  | PApp (SrcSpan a) (QName a) [Pat a]
  | PTuple (SrcSpan a) Boxed [Pat a]
  | PParen (SrcSpan a) (Pat a)
  | PList (SrcSpan a) [Pat a]
  | PWildCard (SrcSpan a)
 deriving Eq

deriving instance ShowAST a => Show (Pat a)

-- | Returns the top-level source span information of a pattern.
instance HasSrcSpan Pat where
  getSrcSpan (PVar srcSpan _)          = srcSpan
  getSrcSpan (PInfixApp srcSpan _ _ _) = srcSpan
  getSrcSpan (PApp srcSpan _ _)        = srcSpan
  getSrcSpan (PTuple srcSpan _ _)      = srcSpan
  getSrcSpan (PParen srcSpan _)        = srcSpan
  getSrcSpan (PList srcSpan _)         = srcSpan
  getSrcSpan (PWildCard srcSpan)       = srcSpan

-- | Converts a pattern to an expression.
patToExp :: Pat a -> Exp a
patToExp (PVar srcSpan name)            = Var srcSpan (unQual name)
patToExp (PInfixApp srcSpan p1 name p2) = InfixApp srcSpan (patToExp p1)
  (qConOp name) (patToExp p2)
patToExp (PApp srcSpan name ps)         = foldl (App srcSpan) (con name)
  (map patToExp ps)
patToExp (PTuple srcSpan boxed ps)      = Tuple srcSpan boxed (map patToExp ps)
patToExp (PParen srcSpan p)             = Paren srcSpan (patToExp p)
patToExp (PList srcSpan ps)             = List srcSpan (map patToExp ps)
patToExp (PWildCard srcSpan)            = Var srcSpan
  (Special srcSpan (ExprHole srcSpan))

-------------------------------------------------------------------------------
-- Names                                                                     --
-------------------------------------------------------------------------------
-- | Type class for AST nodes that can be used in smart constructors where
--   'QName's are expected.
class QNameLike name where
  toQName :: name a -> QName a

-- | A name of a Haskell module used in a qualified name.
data ModuleName a = ModuleName (SrcSpan a) String
 deriving ( Eq, Ord )

deriving instance ShowAST a => Show (ModuleName a)

-- | Gets the source span information of a module name.
instance HasSrcSpan ModuleName where
  getSrcSpan (ModuleName srcSpan _) = srcSpan

-- | A name possibly qualified by a 'ModuleName'.
data QName a
  = Qual (SrcSpan a) (ModuleName a) (Name a)
  | UnQual (SrcSpan a) (Name a)
  | Special (SrcSpan a) (SpecialCon a)
 deriving ( Eq, Ord )

deriving instance ShowAST a => Show (QName a)

-- | Wraps a name with 'UnQual'.
--
--   The additional source span information is taken from the given name.
unQual :: Name a -> QName a
unQual name = UnQual (getSrcSpan name) name

-- | Wraps a name with 'Special'.
--
--   The additional source span information is taken from the given name.
special :: SpecialCon a -> QName a
special specialCon = Special (getSrcSpan specialCon) specialCon

-- | Gets the source span information of a possibly qualified name.
instance HasSrcSpan QName where
  getSrcSpan (Qual srcSpan _ _)  = srcSpan
  getSrcSpan (UnQual srcSpan _)  = srcSpan
  getSrcSpan (Special srcSpan _) = srcSpan

-- | 'QName's can be used everywhere where 'QName's are expected.
instance QNameLike QName where
  toQName = id

-- | An unqualified name.
data Name a = Ident (SrcSpan a) String | Symbol (SrcSpan a) String
 deriving ( Eq, Ord )

deriving instance ShowAST a => Show (Name a)

-- | Gets the source span information of a name.
instance HasSrcSpan Name where
  getSrcSpan (Ident srcSpan _)  = srcSpan
  getSrcSpan (Symbol srcSpan _) = srcSpan

-- | 'Name's can be used everywhere where 'QName's are expected by
--    wrapping them with 'UnQual'.
--
--    The source span information of the 'Name' is copied to the 'QName'.
instance QNameLike Name where
  toQName = unQual

-- | A possibly qualified infix operator.
data QOp a = QVarOp (SrcSpan a) (QName a) | QConOp (SrcSpan a) (QName a)
 deriving ( Eq, Ord )

deriving instance ShowAST a => Show (QOp a)

-- | Creates a variable infix operator from a name.
--
--   The additional source span information is taken from the given name.
qVarOp :: (HasSrcSpan name, QNameLike name) => name a -> QOp a
qVarOp name = QVarOp (getSrcSpan name) (toQName name)

-- | Creates a constructor infix operator from a name.
--
--   The additional source span information is taken from the given name.
qConOp :: (HasSrcSpan name, QNameLike name) => name a -> QOp a
qConOp name = QConOp (getSrcSpan name) (toQName name)

-- | Gets the source span information of a possibly qualified infix operator.
instance HasSrcSpan QOp where
  getSrcSpan (QVarOp srcSpan _) = srcSpan
  getSrcSpan (QConOp srcSpan _) = srcSpan

-- | A built-in data constructor with special syntax.
data SpecialCon a
  = UnitCon (SrcSpan a)
  | UnboxedSingleCon (SrcSpan a)
  | TupleCon (SrcSpan a) Boxed Int
  | NilCon (SrcSpan a)
  | ConsCon (SrcSpan a)
  | ExprHole (SrcSpan a)
 deriving ( Eq, Ord )

deriving instance ShowAST a => Show (SpecialCon a)

-- | Gets the source span information of a built-in constructor with special
--   syntax.
instance HasSrcSpan SpecialCon where
  getSrcSpan (UnitCon srcSpan)          = srcSpan
  getSrcSpan (UnboxedSingleCon srcSpan) = srcSpan
  getSrcSpan (TupleCon srcSpan _ _)     = srcSpan
  getSrcSpan (NilCon srcSpan)           = srcSpan
  getSrcSpan (ConsCon srcSpan)          = srcSpan
  getSrcSpan (ExprHole srcSpan)         = srcSpan

-- | 'SpecialCon'structors can be used everywhere where 'QName's are expected
--   by wrapping them with 'Special'.
--
--   The source span information of the 'SpecialCon' is copied to the 'QName'.
instance QNameLike SpecialCon where
  toQName = special

-------------------------------------------------------------------------------
-- Source Spans                                                              --
-------------------------------------------------------------------------------
-- | Type class for AST nodes with a 'SrcSpan'.
class HasSrcSpan node where
  getSrcSpan :: node a -> SrcSpan a

-- | A wrapper for source span information with the option to not specify a
--   source span. Good source spans contain a 'MsgSrcSpan' which stores basic
--   data about the source span.
--
--   Only the original source span is used when transforming a source span
--   back, all other data is used for displaying input code excerpts.
data SrcSpan a
  = SrcSpan { originalSrcSpan :: SrcSpanType a, actualSrcSpan :: MsgSrcSpan }
  | NoSrcSpan

deriving instance ShowAST a => Show (SrcSpan a)

-- | Custom equality instance for 'SrcSpan' which always returns @True@.
instance Eq (SrcSpan a) where
  _ == _ = True

-- | Custom order for 'SrcSpan' which treats all source spans as equal.
instance Ord (SrcSpan a) where
  _ `compare` _ = EQ

-- | Type for storing basic data of a source span, i. e. the file path and the
--   line and column of the start and end of the spanned source code.
--
--   The data contained in this type is used for displaying input code
--   excerpts. This additional type is necessary in order to have a type for
--   source spans without type variables, which would cause problems in the
--   "HST.Effect.Report" module.
data MsgSrcSpan = MsgSrcSpan
  { msgSrcSpanFilePath    :: FilePath
  , msgSrcSpanStartLine   :: Int
  , msgSrcSpanStartColumn :: Int
  , msgSrcSpanEndLine     :: Int
  , msgSrcSpanEndColumn   :: Int
  }
 deriving ( Eq, Show )

-- | Extracts the 'MsgSrcSpan' of a 'SrcSpan'.
--
--   The result is wrapped inside the @Maybe@ type, so that @Nothing@ can be
--   returned for bad source spans.
toMsgSrcSpan :: SrcSpan a -> Maybe MsgSrcSpan
toMsgSrcSpan (SrcSpan _ msgSrcSpan) = Just msgSrcSpan
toMsgSrcSpan NoSrcSpan              = Nothing
