{-# LANGUAGE TypeFamilies, FlexibleContexts, StandaloneDeriving #-}

module HST.Frontend.Syntax where

-------------------------------------------------------------------------------
-- AST Data Structure                                                        --
-------------------------------------------------------------------------------

type family SrcSpanType a
type family Literal a
type family TypeExp a

class (Eq (Literal a), Eq (TypeExp a)) => EqAST a

class (Show (SrcSpanType a), Show (Literal a), Show (TypeExp a)) => ShowAST a

newtype Module a = Module [Decl a]
deriving instance EqAST a => Eq (Module a)
deriving instance ShowAST a => Show (Module a)

data Decl a = DataDecl (DeclHead a) [ConDecl a]
            | TypeSig (SrcSpan a) [Name a] (TypeExp a)
            | FunBind (SrcSpan a) [Match a]
deriving instance EqAST a => Eq (Decl a)
deriving instance ShowAST a => Show (Decl a)

data DeclHead a = DHead (Name a)
                | DHInfix (Name a)
                | DHParen (DeclHead a)
                | DHApp (DeclHead a)
  deriving Eq
deriving instance ShowAST a => Show (DeclHead a)

data ConDecl a = ConDecl (Name a) [TypeExp a]
               | InfixConDecl (TypeExp a) (Name a) (TypeExp a)
               | RecDecl (Name a)
deriving instance EqAST a => Eq (ConDecl a)
deriving instance ShowAST a => Show (ConDecl a)

data Binds a = BDecls (SrcSpan a) [Decl a]
deriving instance EqAST a => Eq (Binds a)
deriving instance ShowAST a => Show (Binds a)

data Match a
  = Match (SrcSpan a) (Name a) [Pat a] (Rhs a) (Maybe (Binds a))
  | InfixMatch (SrcSpan a) (Pat a) (Name a) [Pat a] (Rhs a) (Maybe (Binds a))
deriving instance EqAST a => Eq (Match a)
deriving instance ShowAST a => Show (Match a)

data Rhs a = UnGuardedRhs (SrcSpan a) (Exp a)
           | GuardedRhss (SrcSpan a) [GuardedRhs a]
deriving instance EqAST a => Eq (Rhs a)
deriving instance ShowAST a => Show (Rhs a)

data GuardedRhs a = GuardedRhs (SrcSpan a) (Exp a) (Exp a)
deriving instance EqAST a => Eq (GuardedRhs a)
deriving instance ShowAST a => Show (GuardedRhs a)

data Boxed = Boxed
           | Unboxed
  deriving (Eq, Show)

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

data Alt a = Alt (SrcSpan a) (Pat a) (Rhs a) (Maybe (Binds a))
deriving instance EqAST a => Eq (Alt a)
deriving instance ShowAST a => Show (Alt a)

data Pat a = PVar (SrcSpan a) (Name a)
           | PInfixApp (SrcSpan a) (Pat a) (QName a) (Pat a)
           | PApp (SrcSpan a) (QName a) [Pat a]
           | PTuple (SrcSpan a) Boxed [Pat a]
           | PParen (SrcSpan a) (Pat a)
           | PList (SrcSpan a) [Pat a]
           | PWildCard (SrcSpan a)
  deriving Eq
deriving instance ShowAST a => Show (Pat a)

data Sign a = Signless (SrcSpan a)
            | Negative (SrcSpan a)
  deriving Eq
deriving instance ShowAST a => Show (Sign a)

data ModuleName a = ModuleName (SrcSpan a) String
  deriving Eq
deriving instance ShowAST a => Show (ModuleName a)

data QName a = Qual (SrcSpan a) (ModuleName a) (Name a)
             | UnQual (SrcSpan a) (Name a)
             | Special (SrcSpan a) (SpecialCon a)
  deriving Eq
deriving instance ShowAST a => Show (QName a)

data Name a = Ident (SrcSpan a) String
            | Symbol (SrcSpan a) String
  deriving Eq
deriving instance ShowAST a => Show (Name a)

data QOp a = QVarOp (SrcSpan a) (QName a)
           | QConOp (SrcSpan a) (QName a)
  deriving Eq
deriving instance ShowAST a => Show (QOp a)

data SpecialCon a = UnitCon (SrcSpan a)
                  | ListCon (SrcSpan a)
                  | FunCon (SrcSpan a)
                  | TupleCon (SrcSpan a) Boxed Int
                  | Cons (SrcSpan a)
                  | UnboxedSingleCon (SrcSpan a)
                  | ExprHole (SrcSpan a)
  deriving Eq
deriving instance ShowAST a => Show (SpecialCon a)

data SrcSpan a = SrcSpan (SrcSpanType a)
               | NoSrcSpan
deriving instance ShowAST a => Show (SrcSpan a)

instance Eq (SrcSpan a) where
  _ == _ = True

-------------------------------------------------------------------------------
-- Construction and Destruction Functions                                    --
-------------------------------------------------------------------------------

var :: Name a -> Exp a
var n@(Ident  s _) = Var s (UnQual s n)
var n@(Symbol s _) = Var s (UnQual s n)

alt :: Pat a -> Exp a -> Alt a
alt pat e = Alt (getSrcPat pat) pat (UnGuardedRhs (getSrcExp e) e) Nothing

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

getSrcPat :: Pat a -> SrcSpan a
getSrcPat (PVar src _         ) = src
getSrcPat (PInfixApp src _ _ _) = src
getSrcPat (PApp   src _ _     ) = src
getSrcPat (PTuple src _ _     ) = src
getSrcPat (PParen src _       ) = src
getSrcPat (PList  src _       ) = src
getSrcPat (PWildCard src      ) = src
