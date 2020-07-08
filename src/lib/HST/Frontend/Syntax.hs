module HST.Frontend.Syntax where

-------------------------------------------------------------------------------
-- AST Data Structure                                                        --
-------------------------------------------------------------------------------

data Module s l t = Module [Decl s l t]
  deriving (Eq, Show)

data Decl s l t = DataDecl (DeclHead s) [ConDecl s t]
                | TypeSig (SrcSpan s) [Name s] t
                | FunBind (SrcSpan s) [Match s l t]
  deriving (Eq, Show)

data DeclHead s = DHead (Name s)
                | DHInfix (Name s)
                | DHParen (DeclHead s)
                | DHApp (DeclHead s)
  deriving (Eq, Show)

data ConDecl s t = ConDecl (Name s) [t]
                 | InfixConDecl t (Name s) t
                 | RecDecl (Name s)
  deriving (Eq, Show)

data Binds s l t = BDecls (SrcSpan s) [Decl s l t]
  deriving (Eq, Show)

data Match s l t
  = Match (SrcSpan s) (Name s) [Pat s l] (Rhs s l t) (Maybe (Binds s l t))
  | InfixMatch (SrcSpan s) (Pat s l) (Name s) [Pat s l]
               (Rhs s l t) (Maybe (Binds s l t))
  deriving (Eq, Show)

data Rhs s l t = UnGuardedRhs (SrcSpan s) (Exp s l t)
               | GuardedRhss (SrcSpan s) [GuardedRhs s l t]
  deriving (Eq, Show)

data GuardedRhs s l t = GuardedRhs (SrcSpan s) (Exp s l t) (Exp s l t)
  deriving (Eq, Show)

data Boxed = Boxed
           | Unboxed
  deriving (Eq, Show)

data Exp s l t = Var (SrcSpan s) (QName s)
               | Con (SrcSpan s) (QName s)
               | Lit (SrcSpan s) l
               | InfixApp (SrcSpan s) (Exp s l t) (QOp s) (Exp s l t)
               | App (SrcSpan s) (Exp s l t) (Exp s l t)
               | NegApp (SrcSpan s) (Exp s l t)
               | Lambda (SrcSpan s) [Pat s l] (Exp s l t)
               | Let (SrcSpan s) (Binds s l t) (Exp s l t)
               | If (SrcSpan s) (Exp s l t) (Exp s l t) (Exp s l t)
               | Case (SrcSpan s) (Exp s l t) [Alt s l t]
               | Tuple (SrcSpan s) Boxed [Exp s l t]
               | List (SrcSpan s) [Exp s l t]
               | Paren (SrcSpan s) (Exp s l t)
               | ExpTypeSig (SrcSpan s) (Exp s l t) t
  deriving (Eq, Show)

data Alt s l t = Alt (SrcSpan s) (Pat s l) (Rhs s l t) (Maybe (Binds s l t))
  deriving (Eq, Show)

data Pat s l = PVar (SrcSpan s) (Name s)
             | PInfixApp (SrcSpan s) (Pat s l) (QName s) (Pat s l)
             | PApp (SrcSpan s) (QName s) [Pat s l]
             | PTuple (SrcSpan s) Boxed [Pat s l]
             | PParen (SrcSpan s) (Pat s l)
             | PList (SrcSpan s) [Pat s l]
             | PWildCard (SrcSpan s)
  deriving (Eq, Show)

data Sign s = Signless (SrcSpan s)
            | Negative (SrcSpan s)
  deriving (Eq, Show)

data ModuleName s = ModuleName (SrcSpan s) String
  deriving (Eq, Show)

data QName s = Qual (SrcSpan s) (ModuleName s) (Name s)
             | UnQual (SrcSpan s) (Name s)
             | Special (SrcSpan s) (SpecialCon s)
  deriving (Eq, Show)

data Name s = Ident (SrcSpan s) String
            | Symbol (SrcSpan s) String
  deriving (Eq, Show)

data QOp s = QVarOp (SrcSpan s) (QName s)
           | QConOp (SrcSpan s) (QName s)
  deriving (Eq, Show)

data SpecialCon s = UnitCon (SrcSpan s)
                  | ListCon (SrcSpan s)
                  | FunCon (SrcSpan s)
                  | TupleCon (SrcSpan s) Boxed Int
                  | Cons (SrcSpan s)
                  | UnboxedSingleCon (SrcSpan s)
                  | ExprHole (SrcSpan s)
  deriving (Eq, Show)

data SrcSpan s = SrcSpan s
               | NoSrcSpan
  deriving Show

instance Eq (SrcSpan s) where
  _ == _ = True

-------------------------------------------------------------------------------
-- Construction and Destruction Functions                                    --
-------------------------------------------------------------------------------

var :: Name s -> Exp s l t
var n@(Ident  s _) = Var s (UnQual s n)
var n@(Symbol s _) = Var s (UnQual s n)

alt :: Pat s l -> Exp s l t -> Alt s l t
alt pat e = Alt (getSrcPat pat) pat (UnGuardedRhs (getSrcExp e) e) Nothing

getSrcExp :: Exp s l t -> SrcSpan s
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

getSrcPat :: Pat s l -> SrcSpan s
getSrcPat (PVar src _         ) = src
getSrcPat (PInfixApp src _ _ _) = src
getSrcPat (PApp   src _ _     ) = src
getSrcPat (PTuple src _ _     ) = src
getSrcPat (PParen src _       ) = src
getSrcPat (PList  src _       ) = src
getSrcPat (PWildCard src      ) = src
