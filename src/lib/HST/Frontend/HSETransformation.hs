module HST.Frontend.HSETransformation where

import qualified Language.Haskell.Exts.Syntax  as HSE
import qualified Language.Haskell.Exts.SrcLoc  as Src
import qualified HST.Frontend.Syntax           as HST


tfHSEtoHSTModule :: HSE.Module s -> HST.Module s (HSE.Literal s) (HSE.Type s)
tfHSEtoHSTModule (HSE.Module _ _ _ _ decls) = HST.Module
  (map tfHSEtoHSTDecl (filter supportedDecls decls))
 where
  supportedDecls (HSE.DataDecl _ _ _ _ _ _) = True
  supportedDecls (HSE.FunBind _ _         ) = True
  supportedDecls (HSE.PatBind _ _ _ _     ) = True
  supportedDecls _                          = False
tfHSEtoHSTModule _ = error "Unsupported Module type"

tfHSEtoHSTDecl :: HSE.Decl s -> HST.Decl s (HSE.Literal s) (HSE.Type s)
tfHSEtoHSTDecl (HSE.DataDecl _ (HSE.DataType _) _ dHead qcds _) =
  HST.DataDecl (tfHSEtoHSTDeclHead dHead) (map tfHSEtoHSTQualConDecl qcds)
tfHSEtoHSTDecl (HSE.TypeSig s names typ) =
  HST.TypeSig (tfHSEtoHSTSrcSpan s) (map tfHSEtoHSTName names) typ
tfHSEtoHSTDecl (HSE.FunBind s matches) =
  HST.FunBind (tfHSEtoHSTSrcSpan s) (map tfHSEtoHSTMatch matches)
tfHSEtoHSTDecl (HSE.PatBind s pat rhs mBinds) = HST.PatBind
  (tfHSEtoHSTSrcSpan s)
  (tfHSEtoHSTPat pat)
  (tfHSEtoHSTRhs rhs)
  (fmap tfHSEtoHSTBinds mBinds)
tfHSEtoHSTDecl _ = error "Unsupported Declaration type"

tfHSEtoHSTDeclHead :: HSE.DeclHead s -> HST.DeclHead s
tfHSEtoHSTDeclHead (HSE.DHead _ dName    ) = HST.DHead (tfHSEtoHSTName dName)
tfHSEtoHSTDeclHead (HSE.DHInfix _ _ dName) = HST.DHInfix (tfHSEtoHSTName dName)
tfHSEtoHSTDeclHead (HSE.DHParen _ dHead) =
  HST.DHParen (tfHSEtoHSTDeclHead dHead)
tfHSEtoHSTDeclHead (HSE.DHApp _ dHead _) = HST.DHApp (tfHSEtoHSTDeclHead dHead)

tfHSEtoHSTQualConDecl :: HSE.QualConDecl s -> HST.ConDecl s (HSE.Type s)
tfHSEtoHSTQualConDecl (HSE.QualConDecl _ _ _ conDecl) =
  tfHSEtoHSTConDecl conDecl

tfHSEtoHSTConDecl :: HSE.ConDecl s -> HST.ConDecl s (HSE.Type s)
tfHSEtoHSTConDecl (HSE.ConDecl _ cName types) =
  HST.ConDecl (tfHSEtoHSTName cName) types
tfHSEtoHSTConDecl (HSE.InfixConDecl _ t1 cName t2) =
  HST.InfixConDecl t1 (tfHSEtoHSTName cName) t2
tfHSEtoHSTConDecl (HSE.RecDecl _ cName _) = HST.RecDecl (tfHSEtoHSTName cName)

tfHSEtoHSTBinds :: HSE.Binds s -> HST.Binds s (HSE.Literal s) (HSE.Type s)
tfHSEtoHSTBinds (HSE.BDecls s decls) =
  HST.BDecls (tfHSEtoHSTSrcSpan s) (map tfHSEtoHSTDecl decls)
tfHSEtoHSTBinds _ = error "Implicit bindings are not supported"

tfHSEtoHSTMatch :: HSE.Match s -> HST.Match s (HSE.Literal s) (HSE.Type s)
tfHSEtoHSTMatch (HSE.Match s name pats rhs mBinds) = HST.Match
  (tfHSEtoHSTSrcSpan s)
  (tfHSEtoHSTName name)
  (map tfHSEtoHSTPat pats)
  (tfHSEtoHSTRhs rhs)
  (fmap tfHSEtoHSTBinds mBinds)
tfHSEtoHSTMatch (HSE.InfixMatch s pat name pats rhs mBinds) = HST.InfixMatch
  (tfHSEtoHSTSrcSpan s)
  (tfHSEtoHSTPat pat)
  (tfHSEtoHSTName name)
  (map tfHSEtoHSTPat pats)
  (tfHSEtoHSTRhs rhs)
  (fmap tfHSEtoHSTBinds mBinds)

tfHSEtoHSTRhs :: HSE.Rhs s -> HST.Rhs s (HSE.Literal s) (HSE.Type s)
tfHSEtoHSTRhs (HSE.UnGuardedRhs s e) =
  HST.UnGuardedRhs (tfHSEtoHSTSrcSpan s) (tfHSEtoHSTExp e)
tfHSEtoHSTRhs (HSE.GuardedRhss s grhss) =
  HST.GuardedRhss (tfHSEtoHSTSrcSpan s) (map tfHSEtoHSTGuardedRhs grhss)

tfHSEtoHSTGuardedRhs
  :: HSE.GuardedRhs s -> HST.GuardedRhs s (HSE.Literal s) (HSE.Type s)
tfHSEtoHSTGuardedRhs (HSE.GuardedRhs s stmts e) = HST.GuardedRhs
  (tfHSEtoHSTSrcSpan s)
  (map tfHSEtoHSTStmt stmts)
  (tfHSEtoHSTExp e)

tfHSEtoHSTBoxed :: HSE.Boxed -> HST.Boxed
tfHSEtoHSTBoxed HSE.Boxed   = HST.Boxed
tfHSEtoHSTBoxed HSE.Unboxed = HST.Unboxed

tfHSEtoHSTExp :: HSE.Exp s -> HST.Exp s (HSE.Literal s) (HSE.Type s)
tfHSEtoHSTExp (HSE.Var s qName) =
  HST.Var (tfHSEtoHSTSrcSpan s) (tfHSEtoHSTQName qName)
tfHSEtoHSTExp (HSE.Con s qName) =
  HST.Con (tfHSEtoHSTSrcSpan s) (tfHSEtoHSTQName qName)
tfHSEtoHSTExp (HSE.Lit s lit           ) = (HST.Lit (tfHSEtoHSTSrcSpan s) lit)
tfHSEtoHSTExp (HSE.InfixApp s e1 qOp e2) = HST.InfixApp (tfHSEtoHSTSrcSpan s)
                                                        (tfHSEtoHSTExp e1)
                                                        (tfHSEtoHSTQOp qOp)
                                                        (tfHSEtoHSTExp e2)
tfHSEtoHSTExp (HSE.App s e1 e2) =
  HST.App (tfHSEtoHSTSrcSpan s) (tfHSEtoHSTExp e1) (tfHSEtoHSTExp e2)
tfHSEtoHSTExp (HSE.NegApp s e) =
  HST.NegApp (tfHSEtoHSTSrcSpan s) (tfHSEtoHSTExp e)
tfHSEtoHSTExp (HSE.Lambda s pats e) =
  HST.Lambda (tfHSEtoHSTSrcSpan s) (map tfHSEtoHSTPat pats) (tfHSEtoHSTExp e)
tfHSEtoHSTExp (HSE.Let s binds e) =
  HST.Let (tfHSEtoHSTSrcSpan s) (tfHSEtoHSTBinds binds) (tfHSEtoHSTExp e)
tfHSEtoHSTExp (HSE.If s e1 e2 e3) = HST.If (tfHSEtoHSTSrcSpan s)
                                           (tfHSEtoHSTExp e1)
                                           (tfHSEtoHSTExp e2)
                                           (tfHSEtoHSTExp e3)
tfHSEtoHSTExp (HSE.Case s e alts) =
  HST.Case (tfHSEtoHSTSrcSpan s) (tfHSEtoHSTExp e) (map tfHSEtoHSTAlt alts)
tfHSEtoHSTExp (HSE.Do s stmts) =
  HST.Do (tfHSEtoHSTSrcSpan s) (map tfHSEtoHSTStmt stmts)
tfHSEtoHSTExp (HSE.Tuple s bxd es) =
  HST.Tuple (tfHSEtoHSTSrcSpan s) (tfHSEtoHSTBoxed bxd) (map tfHSEtoHSTExp es)
tfHSEtoHSTExp (HSE.List s es) =
  HST.List (tfHSEtoHSTSrcSpan s) (map tfHSEtoHSTExp es)
tfHSEtoHSTExp (HSE.Paren s e) =
  HST.Paren (tfHSEtoHSTSrcSpan s) (tfHSEtoHSTExp e)
tfHSEtoHSTExp (HSE.EnumFrom s e) =
  HST.EnumFrom (tfHSEtoHSTSrcSpan s) (tfHSEtoHSTExp e)
tfHSEtoHSTExp (HSE.EnumFromTo s e1 e2) =
  HST.EnumFromTo (tfHSEtoHSTSrcSpan s) (tfHSEtoHSTExp e1) (tfHSEtoHSTExp e2)
tfHSEtoHSTExp (HSE.EnumFromThen s e1 e2) =
  HST.EnumFromThen (tfHSEtoHSTSrcSpan s) (tfHSEtoHSTExp e1) (tfHSEtoHSTExp e2)
tfHSEtoHSTExp (HSE.EnumFromThenTo s e1 e2 e3) = HST.EnumFromThenTo
  (tfHSEtoHSTSrcSpan s)
  (tfHSEtoHSTExp e1)
  (tfHSEtoHSTExp e2)
  (tfHSEtoHSTExp e3)
tfHSEtoHSTExp (HSE.ListComp s e qStmts) = HST.ListComp
  (tfHSEtoHSTSrcSpan s)
  (tfHSEtoHSTExp e)
  (map tfHSEtoHSTQualStmt qStmts)
tfHSEtoHSTExp (HSE.ExpTypeSig s e typ) =
  HST.ExpTypeSig (tfHSEtoHSTSrcSpan s) (tfHSEtoHSTExp e) typ
tfHSEtoHSTExp _ = error "Unsupported Expression type"

tfHSEtoHSTStmt :: HSE.Stmt s -> HST.Stmt s (HSE.Literal s) (HSE.Type s)
tfHSEtoHSTStmt (HSE.Generator s pat e) =
  HST.Generator (tfHSEtoHSTSrcSpan s) (tfHSEtoHSTPat pat) (tfHSEtoHSTExp e)
tfHSEtoHSTStmt (HSE.Qualifier s e) =
  HST.Qualifier (tfHSEtoHSTSrcSpan s) (tfHSEtoHSTExp e)
tfHSEtoHSTStmt (HSE.LetStmt s binds) =
  HST.LetStmt (tfHSEtoHSTSrcSpan s) (tfHSEtoHSTBinds binds)
tfHSEtoHSTStmt (HSE.RecStmt s stmts) =
  HST.RecStmt (tfHSEtoHSTSrcSpan s) (map tfHSEtoHSTStmt stmts)

tfHSEtoHSTQualStmt
  :: HSE.QualStmt s -> HST.QualStmt s (HSE.Literal s) (HSE.Type s)
tfHSEtoHSTQualStmt (HSE.QualStmt s stmt) =
  HST.QualStmt (tfHSEtoHSTSrcSpan s) (tfHSEtoHSTStmt stmt)
tfHSEtoHSTQualStmt _ = error "List Comprehension Extensions are not supported"

tfHSEtoHSTAlt :: HSE.Alt s -> HST.Alt s (HSE.Literal s) (HSE.Type s)
tfHSEtoHSTAlt (HSE.Alt s pat rhs mBinds) = HST.Alt
  (tfHSEtoHSTSrcSpan s)
  (tfHSEtoHSTPat pat)
  (tfHSEtoHSTRhs rhs)
  (fmap tfHSEtoHSTBinds mBinds)

tfHSEtoHSTPat :: HSE.Pat s -> HST.Pat s (HSE.Literal s)
tfHSEtoHSTPat (HSE.PVar s name) =
  HST.PVar (tfHSEtoHSTSrcSpan s) (tfHSEtoHSTName name)
tfHSEtoHSTPat (HSE.PLit s sign lit) =
  HST.PLit (tfHSEtoHSTSrcSpan s) (tfHSEtoHSTSign sign) lit
tfHSEtoHSTPat (HSE.PInfixApp s pat1 qName pat2) = HST.PInfixApp
  (tfHSEtoHSTSrcSpan s)
  (tfHSEtoHSTPat pat1)
  (tfHSEtoHSTQName qName)
  (tfHSEtoHSTPat pat2)
tfHSEtoHSTPat (HSE.PApp s qName pats) = HST.PApp (tfHSEtoHSTSrcSpan s)
                                                 (tfHSEtoHSTQName qName)
                                                 (map tfHSEtoHSTPat pats)
tfHSEtoHSTPat (HSE.PTuple s bxd pats) = HST.PTuple (tfHSEtoHSTSrcSpan s)
                                                   (tfHSEtoHSTBoxed bxd)
                                                   (map tfHSEtoHSTPat pats)
tfHSEtoHSTPat (HSE.PParen s pat) =
  HST.PParen (tfHSEtoHSTSrcSpan s) (tfHSEtoHSTPat pat)
tfHSEtoHSTPat (HSE.PList s pats) =
  HST.PList (tfHSEtoHSTSrcSpan s) (map tfHSEtoHSTPat pats)
tfHSEtoHSTPat (HSE.PWildCard s) = HST.PWildCard (tfHSEtoHSTSrcSpan s)
tfHSEtoHSTPat _                 = error "Unsupported Pattern type"

tfHSEtoHSTSign :: HSE.Sign s -> HST.Sign s
tfHSEtoHSTSign (HSE.Signless s) = HST.Signless (tfHSEtoHSTSrcSpan s)
tfHSEtoHSTSign (HSE.Negative s) = HST.Negative (tfHSEtoHSTSrcSpan s)

tfHSEtoHSTModuleName :: HSE.ModuleName s -> HST.ModuleName s
tfHSEtoHSTModuleName (HSE.ModuleName s name) =
  HST.ModuleName (tfHSEtoHSTSrcSpan s) name

tfHSEtoHSTQName :: HSE.QName s -> HST.QName s
tfHSEtoHSTQName (HSE.Qual s modName name) = HST.Qual
  (tfHSEtoHSTSrcSpan s)
  (tfHSEtoHSTModuleName modName)
  (tfHSEtoHSTName name)
tfHSEtoHSTQName (HSE.UnQual s name) =
  HST.UnQual (tfHSEtoHSTSrcSpan s) (tfHSEtoHSTName name)
tfHSEtoHSTQName (HSE.Special s spCon) =
  HST.Special (tfHSEtoHSTSrcSpan s) (tfHSEtoHSTSpecialCon spCon)

tfHSEtoHSTName :: HSE.Name s -> HST.Name s
tfHSEtoHSTName (HSE.Ident  s name) = HST.Ident (tfHSEtoHSTSrcSpan s) name
tfHSEtoHSTName (HSE.Symbol s name) = HST.Symbol (tfHSEtoHSTSrcSpan s) name

tfHSEtoHSTQOp :: HSE.QOp s -> HST.QOp s
tfHSEtoHSTQOp (HSE.QVarOp s qName) =
  HST.QVarOp (tfHSEtoHSTSrcSpan s) (tfHSEtoHSTQName qName)
tfHSEtoHSTQOp (HSE.QConOp s qName) =
  HST.QConOp (tfHSEtoHSTSrcSpan s) (tfHSEtoHSTQName qName)

tfHSEtoHSTSpecialCon :: HSE.SpecialCon s -> HST.SpecialCon s
tfHSEtoHSTSpecialCon (HSE.UnitCon s) = HST.UnitCon (tfHSEtoHSTSrcSpan s)
tfHSEtoHSTSpecialCon (HSE.ListCon s) = HST.ListCon (tfHSEtoHSTSrcSpan s)
tfHSEtoHSTSpecialCon (HSE.FunCon  s) = HST.FunCon (tfHSEtoHSTSrcSpan s)
tfHSEtoHSTSpecialCon (HSE.TupleCon s bxd n) =
  HST.TupleCon (tfHSEtoHSTSrcSpan s) (tfHSEtoHSTBoxed bxd) n
tfHSEtoHSTSpecialCon (HSE.Cons s) = HST.Cons (tfHSEtoHSTSrcSpan s)
tfHSEtoHSTSpecialCon (HSE.UnboxedSingleCon s) =
  HST.UnboxedSingleCon (tfHSEtoHSTSrcSpan s)
tfHSEtoHSTSpecialCon (HSE.ExprHole s) = HST.ExprHole (tfHSEtoHSTSrcSpan s)

tfHSEtoHSTSrcSpan :: s -> HST.SrcSpan s
tfHSEtoHSTSrcSpan = HST.SrcSpan

tfHSTtoHSEModule
  :: HSE.Module Src.SrcSpanInfo
  -> HST.Module
       Src.SrcSpanInfo
       (HSE.Literal Src.SrcSpanInfo)
       (HSE.Type Src.SrcSpanInfo)
  -> HSE.Module Src.SrcSpanInfo
tfHSTtoHSEModule (HSE.Module srcS mmh pragmas impDecls oDecls) (HST.Module aDecls)
  = HSE.Module
    srcS
    mmh
    pragmas
    impDecls
    (combineDecls oDecls (map tfHSTtoHSEDecl (filter isFunOrPat aDecls)))
 where
  isFunOrPat (HST.FunBind _ _    ) = True
  isFunOrPat (HST.PatBind _ _ _ _) = True
  isFunOrPat _                     = False
  combineDecls (HSE.FunBind _ _ : oDecls') (aDecl : aDecls') =
    aDecl : combineDecls oDecls' aDecls'
  combineDecls (HSE.PatBind _ _ _ _ : oDecls') (aDecl : aDecls') =
    aDecl : combineDecls oDecls' aDecls'
  combineDecls (oDecl : oDecls') aDecls' = oDecl : combineDecls oDecls' aDecls'
  combineDecls []                aDecls' = aDecls'
tfHSTtoHSEModule _ _ = error "Unsupported Module type"


tfHSTtoHSEDecl
  :: HST.Decl
       Src.SrcSpanInfo
       (HSE.Literal Src.SrcSpanInfo)
       (HSE.Type Src.SrcSpanInfo)
  -> HSE.Decl Src.SrcSpanInfo
tfHSTtoHSEDecl (HST.DataDecl _ _) =
  error "Data type declarations should not be transformed back"
tfHSTtoHSEDecl (HST.TypeSig s names typ) =
  HSE.TypeSig (tfHSTtoHSESrcSpan s) (map tfHSTtoHSEName names) typ
tfHSTtoHSEDecl (HST.FunBind s matches) =
  HSE.FunBind (tfHSTtoHSESrcSpan s) (map tfHSTtoHSEMatch matches)
tfHSTtoHSEDecl (HST.PatBind s pat rhs mBinds) = HSE.PatBind
  (tfHSTtoHSESrcSpan s)
  (tfHSTtoHSEPat pat)
  (tfHSTtoHSERhs rhs)
  (fmap tfHSTtoHSEBinds mBinds)

tfHSTtoHSEBinds
  :: HST.Binds
       Src.SrcSpanInfo
       (HSE.Literal Src.SrcSpanInfo)
       (HSE.Type Src.SrcSpanInfo)
  -> HSE.Binds Src.SrcSpanInfo
tfHSTtoHSEBinds (HST.BDecls s decls) =
  HSE.BDecls (tfHSTtoHSESrcSpan s) (map tfHSTtoHSEDecl decls)

tfHSTtoHSEMatch
  :: HST.Match
       Src.SrcSpanInfo
       (HSE.Literal Src.SrcSpanInfo)
       (HSE.Type Src.SrcSpanInfo)
  -> HSE.Match Src.SrcSpanInfo
tfHSTtoHSEMatch (HST.Match s name pats rhs mBinds) = HSE.Match
  (tfHSTtoHSESrcSpan s)
  (tfHSTtoHSEName name)
  (map tfHSTtoHSEPat pats)
  (tfHSTtoHSERhs rhs)
  (fmap tfHSTtoHSEBinds mBinds)
tfHSTtoHSEMatch (HST.InfixMatch s pat name pats rhs mBinds) = HSE.InfixMatch
  (tfHSTtoHSESrcSpan s)
  (tfHSTtoHSEPat pat)
  (tfHSTtoHSEName name)
  (map tfHSTtoHSEPat pats)
  (tfHSTtoHSERhs rhs)
  (fmap tfHSTtoHSEBinds mBinds)

tfHSTtoHSERhs
  :: HST.Rhs
       Src.SrcSpanInfo
       (HSE.Literal Src.SrcSpanInfo)
       (HSE.Type Src.SrcSpanInfo)
  -> HSE.Rhs Src.SrcSpanInfo
tfHSTtoHSERhs (HST.UnGuardedRhs s e) =
  HSE.UnGuardedRhs (tfHSTtoHSESrcSpan s) (tfHSTtoHSEExp e)
tfHSTtoHSERhs (HST.GuardedRhss s grhss) =
  HSE.GuardedRhss (tfHSTtoHSESrcSpan s) (map tfHSTtoHSEGuardedRhs grhss)

tfHSTtoHSEGuardedRhs
  :: HST.GuardedRhs
       Src.SrcSpanInfo
       (HSE.Literal Src.SrcSpanInfo)
       (HSE.Type Src.SrcSpanInfo)
  -> HSE.GuardedRhs Src.SrcSpanInfo
tfHSTtoHSEGuardedRhs (HST.GuardedRhs s stmts e) = HSE.GuardedRhs
  (tfHSTtoHSESrcSpan s)
  (map tfHSTtoHSEStmt stmts)
  (tfHSTtoHSEExp e)

tfHSTtoHSEBoxed :: HST.Boxed -> HSE.Boxed
tfHSTtoHSEBoxed HST.Boxed   = HSE.Boxed
tfHSTtoHSEBoxed HST.Unboxed = HSE.Unboxed

tfHSTtoHSEExp
  :: HST.Exp
       Src.SrcSpanInfo
       (HSE.Literal Src.SrcSpanInfo)
       (HSE.Type Src.SrcSpanInfo)
  -> HSE.Exp Src.SrcSpanInfo
tfHSTtoHSEExp (HST.Var s qName) =
  HSE.Var (tfHSTtoHSESrcSpan s) (tfHSTtoHSEQName qName)
tfHSTtoHSEExp (HST.Con s qName) =
  HSE.Con (tfHSTtoHSESrcSpan s) (tfHSTtoHSEQName qName)
tfHSTtoHSEExp (HST.Lit s lit           ) = (HSE.Lit (tfHSTtoHSESrcSpan s) lit)
tfHSTtoHSEExp (HST.InfixApp s e1 qOp e2) = HSE.InfixApp (tfHSTtoHSESrcSpan s)
                                                        (tfHSTtoHSEExp e1)
                                                        (tfHSTtoHSEQOp qOp)
                                                        (tfHSTtoHSEExp e2)
tfHSTtoHSEExp (HST.App s e1 e2) =
  HSE.App (tfHSTtoHSESrcSpan s) (tfHSTtoHSEExp e1) (tfHSTtoHSEExp e2)
tfHSTtoHSEExp (HST.NegApp s e) =
  HSE.NegApp (tfHSTtoHSESrcSpan s) (tfHSTtoHSEExp e)
tfHSTtoHSEExp (HST.Lambda s pats e) =
  HSE.Lambda (tfHSTtoHSESrcSpan s) (map tfHSTtoHSEPat pats) (tfHSTtoHSEExp e)
tfHSTtoHSEExp (HST.Let s binds e) =
  HSE.Let (tfHSTtoHSESrcSpan s) (tfHSTtoHSEBinds binds) (tfHSTtoHSEExp e)
tfHSTtoHSEExp (HST.If s e1 e2 e3) = HSE.If (tfHSTtoHSESrcSpan s)
                                           (tfHSTtoHSEExp e1)
                                           (tfHSTtoHSEExp e2)
                                           (tfHSTtoHSEExp e3)
tfHSTtoHSEExp (HST.Case s e alts) =
  HSE.Case (tfHSTtoHSESrcSpan s) (tfHSTtoHSEExp e) (map tfHSTtoHSEAlt alts)
tfHSTtoHSEExp (HST.Do s stmts) =
  HSE.Do (tfHSTtoHSESrcSpan s) (map tfHSTtoHSEStmt stmts)
tfHSTtoHSEExp (HST.Tuple s bxd es) =
  HSE.Tuple (tfHSTtoHSESrcSpan s) (tfHSTtoHSEBoxed bxd) (map tfHSTtoHSEExp es)
tfHSTtoHSEExp (HST.List s es) =
  HSE.List (tfHSTtoHSESrcSpan s) (map tfHSTtoHSEExp es)
tfHSTtoHSEExp (HST.Paren s e) =
  HSE.Paren (tfHSTtoHSESrcSpan s) (tfHSTtoHSEExp e)
tfHSTtoHSEExp (HST.EnumFrom s e) =
  HSE.EnumFrom (tfHSTtoHSESrcSpan s) (tfHSTtoHSEExp e)
tfHSTtoHSEExp (HST.EnumFromTo s e1 e2) =
  HSE.EnumFromTo (tfHSTtoHSESrcSpan s) (tfHSTtoHSEExp e1) (tfHSTtoHSEExp e2)
tfHSTtoHSEExp (HST.EnumFromThen s e1 e2) =
  HSE.EnumFromThen (tfHSTtoHSESrcSpan s) (tfHSTtoHSEExp e1) (tfHSTtoHSEExp e2)
tfHSTtoHSEExp (HST.EnumFromThenTo s e1 e2 e3) = HSE.EnumFromThenTo
  (tfHSTtoHSESrcSpan s)
  (tfHSTtoHSEExp e1)
  (tfHSTtoHSEExp e2)
  (tfHSTtoHSEExp e3)
tfHSTtoHSEExp (HST.ListComp s e qStmts) = HSE.ListComp
  (tfHSTtoHSESrcSpan s)
  (tfHSTtoHSEExp e)
  (map tfHSTtoHSEQualStmt qStmts)
tfHSTtoHSEExp (HST.ExpTypeSig s e typ) =
  HSE.ExpTypeSig (tfHSTtoHSESrcSpan s) (tfHSTtoHSEExp e) typ

tfHSTtoHSEStmt
  :: HST.Stmt
       Src.SrcSpanInfo
       (HSE.Literal Src.SrcSpanInfo)
       (HSE.Type Src.SrcSpanInfo)
  -> HSE.Stmt Src.SrcSpanInfo
tfHSTtoHSEStmt (HST.Generator s pat e) =
  HSE.Generator (tfHSTtoHSESrcSpan s) (tfHSTtoHSEPat pat) (tfHSTtoHSEExp e)
tfHSTtoHSEStmt (HST.Qualifier s e) =
  HSE.Qualifier (tfHSTtoHSESrcSpan s) (tfHSTtoHSEExp e)
tfHSTtoHSEStmt (HST.LetStmt s binds) =
  HSE.LetStmt (tfHSTtoHSESrcSpan s) (tfHSTtoHSEBinds binds)
tfHSTtoHSEStmt (HST.RecStmt s stmts) =
  HSE.RecStmt (tfHSTtoHSESrcSpan s) (map tfHSTtoHSEStmt stmts)

tfHSTtoHSEQualStmt
  :: HST.QualStmt
       Src.SrcSpanInfo
       (HSE.Literal Src.SrcSpanInfo)
       (HSE.Type Src.SrcSpanInfo)
  -> HSE.QualStmt Src.SrcSpanInfo
tfHSTtoHSEQualStmt (HST.QualStmt s stmt) =
  HSE.QualStmt (tfHSTtoHSESrcSpan s) (tfHSTtoHSEStmt stmt)

tfHSTtoHSEAlt
  :: HST.Alt
       Src.SrcSpanInfo
       (HSE.Literal Src.SrcSpanInfo)
       (HSE.Type Src.SrcSpanInfo)
  -> HSE.Alt Src.SrcSpanInfo
tfHSTtoHSEAlt (HST.Alt s pat rhs mBinds) = HSE.Alt
  (tfHSTtoHSESrcSpan s)
  (tfHSTtoHSEPat pat)
  (tfHSTtoHSERhs rhs)
  (fmap tfHSTtoHSEBinds mBinds)

tfHSTtoHSEPat
  :: HST.Pat Src.SrcSpanInfo (HSE.Literal Src.SrcSpanInfo)
  -> HSE.Pat Src.SrcSpanInfo
tfHSTtoHSEPat (HST.PVar s name) =
  HSE.PVar (tfHSTtoHSESrcSpan s) (tfHSTtoHSEName name)
tfHSTtoHSEPat (HST.PLit s sign lit) =
  HSE.PLit (tfHSTtoHSESrcSpan s) (tfHSTtoHSESign sign) lit
tfHSTtoHSEPat (HST.PInfixApp s pat1 qName pat2) = HSE.PInfixApp
  (tfHSTtoHSESrcSpan s)
  (tfHSTtoHSEPat pat1)
  (tfHSTtoHSEQName qName)
  (tfHSTtoHSEPat pat2)
tfHSTtoHSEPat (HST.PApp s qName pats) = HSE.PApp (tfHSTtoHSESrcSpan s)
                                                 (tfHSTtoHSEQName qName)
                                                 (map tfHSTtoHSEPat pats)
tfHSTtoHSEPat (HST.PTuple s bxd pats) = HSE.PTuple (tfHSTtoHSESrcSpan s)
                                                   (tfHSTtoHSEBoxed bxd)
                                                   (map tfHSTtoHSEPat pats)
tfHSTtoHSEPat (HST.PParen s pat) =
  HSE.PParen (tfHSTtoHSESrcSpan s) (tfHSTtoHSEPat pat)
tfHSTtoHSEPat (HST.PList s pats) =
  HSE.PList (tfHSTtoHSESrcSpan s) (map tfHSTtoHSEPat pats)
tfHSTtoHSEPat (HST.PWildCard s) = HSE.PWildCard (tfHSTtoHSESrcSpan s)

tfHSTtoHSESign :: HST.Sign Src.SrcSpanInfo -> HSE.Sign Src.SrcSpanInfo
tfHSTtoHSESign (HST.Signless s) = HSE.Signless (tfHSTtoHSESrcSpan s)
tfHSTtoHSESign (HST.Negative s) = HSE.Negative (tfHSTtoHSESrcSpan s)

tfHSTtoHSEModuleName
  :: HST.ModuleName Src.SrcSpanInfo -> HSE.ModuleName Src.SrcSpanInfo
tfHSTtoHSEModuleName (HST.ModuleName s name) =
  HSE.ModuleName (tfHSTtoHSESrcSpan s) name

tfHSTtoHSEQName :: HST.QName Src.SrcSpanInfo -> HSE.QName Src.SrcSpanInfo
tfHSTtoHSEQName (HST.Qual s modName name) = HSE.Qual
  (tfHSTtoHSESrcSpan s)
  (tfHSTtoHSEModuleName modName)
  (tfHSTtoHSEName name)
tfHSTtoHSEQName (HST.UnQual s name) =
  HSE.UnQual (tfHSTtoHSESrcSpan s) (tfHSTtoHSEName name)
tfHSTtoHSEQName (HST.Special s spCon) =
  HSE.Special (tfHSTtoHSESrcSpan s) (tfHSTtoHSESpecialCon spCon)

tfHSTtoHSEName :: HST.Name Src.SrcSpanInfo -> HSE.Name Src.SrcSpanInfo
tfHSTtoHSEName (HST.Ident  s name) = HSE.Ident (tfHSTtoHSESrcSpan s) name
tfHSTtoHSEName (HST.Symbol s name) = HSE.Symbol (tfHSTtoHSESrcSpan s) name

tfHSTtoHSEQOp :: HST.QOp Src.SrcSpanInfo -> HSE.QOp Src.SrcSpanInfo
tfHSTtoHSEQOp (HST.QVarOp s qName) =
  HSE.QVarOp (tfHSTtoHSESrcSpan s) (tfHSTtoHSEQName qName)
tfHSTtoHSEQOp (HST.QConOp s qName) =
  HSE.QConOp (tfHSTtoHSESrcSpan s) (tfHSTtoHSEQName qName)

tfHSTtoHSESpecialCon
  :: HST.SpecialCon Src.SrcSpanInfo -> HSE.SpecialCon Src.SrcSpanInfo
tfHSTtoHSESpecialCon (HST.UnitCon s) = HSE.UnitCon (tfHSTtoHSESrcSpan s)
tfHSTtoHSESpecialCon (HST.ListCon s) = HSE.ListCon (tfHSTtoHSESrcSpan s)
tfHSTtoHSESpecialCon (HST.FunCon  s) = HSE.FunCon (tfHSTtoHSESrcSpan s)
tfHSTtoHSESpecialCon (HST.TupleCon s bxd n) =
  HSE.TupleCon (tfHSTtoHSESrcSpan s) (tfHSTtoHSEBoxed bxd) n
tfHSTtoHSESpecialCon (HST.Cons s) = HSE.Cons (tfHSTtoHSESrcSpan s)
tfHSTtoHSESpecialCon (HST.UnboxedSingleCon s) =
  HSE.UnboxedSingleCon (tfHSTtoHSESrcSpan s)
tfHSTtoHSESpecialCon (HST.ExprHole s) = HSE.ExprHole (tfHSTtoHSESrcSpan s)

tfHSTtoHSESrcSpan :: HST.SrcSpan Src.SrcSpanInfo -> Src.SrcSpanInfo
tfHSTtoHSESrcSpan s = case s of
  HST.SrcSpan srcSpan -> srcSpan
  HST.NoSrcSpan       -> Src.noSrcSpan
