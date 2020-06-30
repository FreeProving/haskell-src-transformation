module HST.Backend.Syntax where

-- Is the Src Span needed everywhere?
data Module s l t = Module [Decl s l t]
  deriving (Eq, Show)

data Decl s l t = TypeSig s [Name s] t
                | FunBind s [Match s l t]
  deriving (Eq, Show)

data Binds s l t = BDecls s [Decl s l t]
  deriving (Eq, Show)

data Match s l t = Match s (Name s) [Pat s l] (Rhs s l t) (Maybe (Binds s l t))
                 | InfixMatch s (Pat s l) (Name s) [Pat s l] (Rhs s l t) (Maybe (Binds s l t))
  deriving (Eq, Show)

data Rhs s l t = UnGuardedRhs s (Exp s l t)
               | GuardedRhss s [GuardedRhs s l t]
  deriving (Eq, Show)

data GuardedRhs s l t = GuardedRhs s [Stmt s l t] (Exp s l t)
  deriving (Eq, Show)

data Boxed = Boxed
           | Unboxed
  deriving (Eq, Show)

data Exp s l t = Var s (QName s)
               | Con s (QName s)
               | Lit s l
               | InfixApp s (Exp s l t) (QOp s) (Exp s l t)
               | App s (Exp s l t) (Exp s l t)
               | NegApp s (Exp s l t)
               | Lambda s [Pat s l] (Exp s l t)
               | Let s (Binds s l t) (Exp s l t)
               | If s (Exp s l t) (Exp s l t) (Exp s l t)
               | Case s (Exp s l t) [Alt s l t]
               | Do s [Stmt s l t]
               | Tuple s Boxed [Exp s l t]
               | List s [Exp s l t]
               | Paren s (Exp s l t)
               | EnumFrom s (Exp s l t)
               | EnumFromTo s (Exp s l t) (Exp s l t)
               | EnumFromThen s (Exp s l t) (Exp s l t)
               | EnumFromThenTo s (Exp s l t) (Exp s l t) (Exp s l t)
               | ListComp s (Exp s l t) [QualStmt s l t]
               | ExpTypeSig s (Exp s l t) t
  deriving (Eq, Show)

data Stmt s l t = Generator s (Exp s l t)
                | Qualifier s (Exp s l t)
                | LetStmt s (Binds s l t)
                | RectStmt s [Stmt s l t]
  deriving (Eq, Show)

-- Extensions are missing
data QualStmt s l t = QualStmt s (Stmt s l t)
  deriving (Eq, Show)

data Alt s l t = Alt s (Pat s l) (Rhs s l t) (Maybe (Binds s l t))
  deriving (Eq, Show)

data Pat s l = PVar s (Name s)
             | PLit s (Sign s) l
             | PInfixApp s (Pat s l) (QName s) (Pat s l)
             | PApp s (QName s) [Pat s l]
             | PTuple s Boxed [Pat s l]
             | PParen s (Pat s l)
             | PList s [Pat s l]
             | PWildCard s
  deriving (Eq, Show)

data Sign s = Signless s
            | Negative s
  deriving (Eq, Show)

data ModuleName s = ModuleName s String
  deriving (Eq, Show)

data QName s = Qual s (ModuleName s) (Name s)
             | UnQual s (Name s)
             | Special s (SpecialCon s)
  deriving (Eq, Show)

data Name s = Ident s String
            | Symbol s String
  deriving (Eq, Show)

data QOp s = QVarOp s (QName s)
           | QConOp s (QName s)
  deriving (Eq, Show)

data SpecialCon s = UnitCon s
                  | ListCon s
                  | FunCon s
                  | TupleCon s Boxed Int
                  | Cons s
                  | UnboxedSingleCon s
                  | ExprHole s
  deriving (Eq, Show)
