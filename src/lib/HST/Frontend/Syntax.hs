{-# LANGUAGE TypeFamilies #-}

module HST.Frontend.Syntax where

-------------------------------------------------------------------------------
-- AST Data Structure                                                        --
-------------------------------------------------------------------------------

type family SrcSpanType a
type family Literal a
type family TypeExp a

newtype Module a = Module [Decl a]

data Decl a = DataDecl (DeclHead a) [ConDecl a]
            | TypeSig (SrcSpan a) [Name a] (TypeExp a)
            | FunBind (SrcSpan a) [Match a]

data DeclHead a = DHead (Name a)
                | DHInfix (Name a)
                | DHParen (DeclHead a)
                | DHApp (DeclHead a)

data ConDecl a = ConDecl (Name a) [TypeExp a]
               | InfixConDecl (TypeExp a) (Name a) (TypeExp a)
               | RecDecl (Name a)

data Binds a = BDecls (SrcSpan a) [Decl a]

data Match a
  = Match (SrcSpan a) (Name a) [Pat a] (Rhs a) (Maybe (Binds a))
  | InfixMatch (SrcSpan a) (Pat a) (Name a) [Pat a] (Rhs a) (Maybe (Binds a))

data Rhs a = UnGuardedRhs (SrcSpan a) (Exp a)
           | GuardedRhss (SrcSpan a) [GuardedRhs a]

data GuardedRhs a = GuardedRhs (SrcSpan a) (Exp a) (Exp a)

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

data Alt a = Alt (SrcSpan a) (Pat a) (Rhs a) (Maybe (Binds a))

data Pat a = PVar (SrcSpan a) (Name a)
           | PInfixApp (SrcSpan a) (Pat a) (QName a) (Pat a)
           | PApp (SrcSpan a) (QName a) [Pat a]
           | PTuple (SrcSpan a) Boxed [Pat a]
           | PParen (SrcSpan a) (Pat a)
           | PList (SrcSpan a) [Pat a]
           | PWildCard (SrcSpan a)
  deriving Eq

data Sign a = Signless (SrcSpan a)
            | Negative (SrcSpan a)
  deriving Eq

data ModuleName a = ModuleName (SrcSpan a) String
  deriving Eq

data QName a = Qual (SrcSpan a) (ModuleName a) (Name a)
             | UnQual (SrcSpan a) (Name a)
             | Special (SrcSpan a) (SpecialCon a)
  deriving Eq

data Name a = Ident (SrcSpan a) String
            | Symbol (SrcSpan a) String
  deriving Eq

data QOp a = QVarOp (SrcSpan a) (QName a)
           | QConOp (SrcSpan a) (QName a)
  deriving Eq

data SpecialCon a = UnitCon (SrcSpan a)
                  | ListCon (SrcSpan a)
                  | FunCon (SrcSpan a)
                  | TupleCon (SrcSpan a) Boxed Int
                  | Cons (SrcSpan a)
                  | UnboxedSingleCon (SrcSpan a)
                  | ExprHole (SrcSpan a)
  deriving Eq

data SrcSpan a = SrcSpan (SrcSpanType a)
               | NoSrcSpan

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
