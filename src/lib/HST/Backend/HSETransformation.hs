module HST.Backend.HSETransformation where

import qualified Language.Haskell.Exts.Syntax  as HSE
import qualified HST.Backend.Syntax            as HST


tfHSEtoHSTModule :: HSE.Module s -> HST.Module s (HSE.Literal s) (HSE.Type s)
tfHSEtoHSTModule (HSE.Module _ _ _ _ decls) = HST.Module
  (map tfHSEtoHSTDecl (filter isFunBind decls))
 where
  isFunBind (HSE.FunBind _ _) = True
  isFunBind _                 = False
tfHSEtoHSTModule _ = error "Unsupported Module type"

tfHSEtoHSTDecl :: HSE.Decl s -> HST.Decl s (HSE.Literal s) (HSE.Type s)
tfHSEtoHSTDecl (HSE.FunBind srcS matches) =
  HST.FunBind srcS (map tfHSEtoHSTMatch matches)
tfHSEtoHSTDecl (HSE.TypeSig srcS names typ) =
  HST.TypeSig srcS (map tfHSEtoHSTName names) typ
tfHSEtoHSTDecl _ = error "Unsupported Declaration type"

tfHSEtoHSTBinds :: HSE.Binds s -> HST.Binds s (HSE.Literal s) (HSE.Type s)
tfHSEtoHSTBinds (HSE.BDecls srcS decls) =
  HST.BDecls srcS (map tfHSEtoHSTDecl decls)
tfHSEtoHSTBinds _ = error "Implicit bindings are not supported"

tfHSEtoHSTMatch :: HSE.Match s -> HST.Match s (HSE.Literal s) (HSE.Type s)
tfHSEtoHSTMatch (HSE.Match srcS name pats rhs mBinds) = HST.Match
  srcS
  (tfHSEtoHSTName name)
  (map tfHSEtoHSTPat pats)
  (tfHSEtoHSTRhs rhs)
  (fmap tfHSEtoHSTBinds mBinds)
tfHSEtoHSTMatch (HSE.InfixMatch srcS pat name pats rhs mBinds) = HST.InfixMatch
  srcS
  (tfHSEtoHSTPat pat)
  (tfHSEtoHSTName name)
  (map tfHSEtoHSTPat pats)
  (tfHSEtoHSTRhs rhs)
  (fmap tfHSEtoHSTBinds mBinds)

tfHSEtoHSTRhs :: HSE.Rhs s -> HST.Rhs s (HSE.Literal s) (HSE.Type s)
tfHSEtoHSTRhs (HSE.UnGuardedRhs srcS e) =
  HST.UnGuardedRhs srcS (tfHSEtoHSTExp e)
tfHSEtoHSTRhs (HSE.GuardedRhss srcS grhss) =
  HST.GuardedRhss srcS (map tfHSEtoHSTGuardedRhs grhss)

tfHSEtoHSTGuardedRhs
  :: HSE.GuardedRhs s -> HST.GuardedRhs s (HSE.Literal s) (HSE.Type s)
tfHSEtoHSTGuardedRhs (HSE.GuardedRhs srcS stmts e) =
  HST.GuardedRhs srcS (map tfHSEtoHSTStmt stmts) (tfHSEtoHSTExp e)

tfHSEtoHSTBoxed :: HSE.Boxed -> HST.Boxed
tfHSEtoHSTBoxed HSE.Boxed   = HST.Boxed
tfHSEtoHSTBoxed HSE.Unboxed = HST.Unboxed

tfHSEtoHSTExp :: HSE.Exp s -> HST.Exp s (HSE.Literal s) (HSE.Type s)
tfHSEtoHSTExp (HSE.Var srcS qName) = HST.Var srcS (tfHSEtoHSTQName qName)
tfHSEtoHSTExp (HSE.Con srcS qName) = HST.Con srcS (tfHSEtoHSTQName qName)
tfHSEtoHSTExp (HSE.InfixApp srcS e1 qOp e2) =
  HST.InfixApp srcS (tfHSEtoHSTExp e1) (tfHSEtoHSTQOp qOp) (tfHSEtoHSTExp e2)
tfHSEtoHSTExp (HSE.App srcS e1 e2) =
  HST.App srcS (tfHSEtoHSTExp e1) (tfHSEtoHSTExp e2)
tfHSEtoHSTExp (HSE.NegApp srcS e) = HST.NegApp srcS (tfHSEtoHSTExp e)
tfHSEtoHSTExp (HSE.Lambda srcS pats e) =
  HST.Lambda srcS (map tfHSEtoHSTPat pats) (tfHSEtoHSTExp e)
tfHSEtoHSTExp (HSE.Let srcS binds e) =
  HST.Let srcS (tfHSEtoHSTBinds binds) (tfHSEtoHSTExp e)
tfHSEtoHSTExp (HSE.If srcS e1 e2 e3) =
  HST.If srcS (tfHSEtoHSTExp e1) (tfHSEtoHSTExp e2) (tfHSEtoHSTExp e3)
tfHSEtoHSTExp (HSE.Case srcS e alts) =
  HST.Case srcS (tfHSEtoHSTExp e) (map tfHSEtoHSTAlt alts)
tfHSEtoHSTExp (HSE.Do srcS stmts) = HST.Do srcS (map tfHSEtoHSTStmt stmts)
tfHSEtoHSTExp (HSE.Tuple srcS bxd es) =
  HST.Tuple srcS (tfHSEtoHSTBoxed bxd) (map tfHSEtoHSTExp es)
tfHSEtoHSTExp (HSE.List     srcS es) = HST.List srcS (map tfHSEtoHSTExp es)
tfHSEtoHSTExp (HSE.Paren    srcS e ) = HST.Paren srcS (tfHSEtoHSTExp e)
tfHSEtoHSTExp (HSE.EnumFrom srcS e ) = HST.EnumFrom srcS (tfHSEtoHSTExp e)
tfHSEtoHSTExp (HSE.EnumFromTo srcS e1 e2) =
  HST.EnumFromTo srcS (tfHSEtoHSTExp e1) (tfHSEtoHSTExp e2)
tfHSEtoHSTExp (HSE.EnumFromThen srcS e1 e2) =
  HST.EnumFromThen srcS (tfHSEtoHSTExp e1) (tfHSEtoHSTExp e2)
tfHSEtoHSTExp (HSE.EnumFromThenTo srcS e1 e2 e3) = HST.EnumFromThenTo
  srcS
  (tfHSEtoHSTExp e1)
  (tfHSEtoHSTExp e2)
  (tfHSEtoHSTExp e3)
tfHSEtoHSTExp (HSE.ListComp srcS e qStmts) =
  HST.ListComp srcS (tfHSEtoHSTExp e) (map tfHSEtoHSTQualStmt qStmts)
tfHSEtoHSTExp (HSE.ExpTypeSig srcS e typ) =
  HST.ExpTypeSig srcS (tfHSEtoHSTExp e) typ
tfHSEtoHSTExp _ = error "Unsupported Expression type"

tfHSEtoHSTStmt :: HSE.Stmt s -> HST.Stmt s (HSE.Literal s) (HSE.Type s)
tfHSEtoHSTStmt (HSE.Generator srcS pat e) =
  HST.Generator srcS (tfHSEtoHSTPat pat) (tfHSEtoHSTExp e)
tfHSEtoHSTStmt (HSE.Qualifier srcS e) = HST.Qualifier srcS (tfHSEtoHSTExp e)
tfHSEtoHSTStmt (HSE.LetStmt srcS binds) =
  HST.LetStmt srcS (tfHSEtoHSTBinds binds)
tfHSEtoHSTStmt (HSE.RecStmt srcS stmts) =
  HST.RecStmt srcS (map tfHSEtoHSTStmt stmts)

tfHSEtoHSTQualStmt
  :: HSE.QualStmt s -> HST.QualStmt s (HSE.Literal s) (HSE.Type s)
tfHSEtoHSTQualStmt (HSE.QualStmt srcS stmt) =
  HST.QualStmt srcS (tfHSEtoHSTStmt stmt)
tfHSEtoHSTQualStmt _ = error "List Comprehension Extensions are not supported"

tfHSEtoHSTAlt :: HSE.Alt s -> HST.Alt s (HSE.Literal s) (HSE.Type s)
tfHSEtoHSTAlt (HSE.Alt srcS pat rhs mBinds) = HST.Alt
  srcS
  (tfHSEtoHSTPat pat)
  (tfHSEtoHSTRhs rhs)
  (fmap tfHSEtoHSTBinds mBinds)

tfHSEtoHSTPat :: HSE.Pat s -> HST.Pat s (HSE.Literal s)
tfHSEtoHSTPat (HSE.PVar srcS name) = HST.PVar srcS (tfHSEtoHSTName name)
tfHSEtoHSTPat (HSE.PLit srcS sign lit) =
  HST.PLit srcS (tfHSEtoHSTSign sign) lit
tfHSEtoHSTPat (HSE.PInfixApp srcS pat1 qName pat2) = HST.PInfixApp
  srcS
  (tfHSEtoHSTPat pat1)
  (tfHSEtoHSTQName qName)
  (tfHSEtoHSTPat pat2)
tfHSEtoHSTPat (HSE.PApp srcS qName pats) =
  HST.PApp srcS (tfHSEtoHSTQName qName) (map tfHSEtoHSTPat pats)
tfHSEtoHSTPat (HSE.PTuple srcS bxd pats) =
  HST.PTuple srcS (tfHSEtoHSTBoxed bxd) (map tfHSEtoHSTPat pats)
tfHSEtoHSTPat (HSE.PParen srcS pat ) = HST.PParen srcS (tfHSEtoHSTPat pat)
tfHSEtoHSTPat (HSE.PList  srcS pats) = HST.PList srcS (map tfHSEtoHSTPat pats)
tfHSEtoHSTPat (HSE.PWildCard srcS  ) = HST.PWildCard srcS
tfHSEtoHSTPat _                      = error "Unsupported Pattern type"

tfHSEtoHSTSign :: HSE.Sign s -> HST.Sign s
tfHSEtoHSTSign (HSE.Signless srcS) = HST.Signless srcS
tfHSEtoHSTSign (HSE.Negative srcS) = HST.Negative srcS

tfHSEtoHSTModuleName :: HSE.ModuleName s -> HST.ModuleName s
tfHSEtoHSTModuleName (HSE.ModuleName srcS name) = HST.ModuleName srcS name

tfHSEtoHSTQName :: HSE.QName s -> HST.QName s
tfHSEtoHSTQName (HSE.Qual srcS modName name) =
  HST.Qual srcS (tfHSEtoHSTModuleName modName) (tfHSEtoHSTName name)
tfHSEtoHSTQName (HSE.UnQual srcS name) = HST.UnQual srcS (tfHSEtoHSTName name)
tfHSEtoHSTQName (HSE.Special srcS spCon) =
  HST.Special srcS (tfHSEtoHSTSpecialCon spCon)

tfHSEtoHSTName :: HSE.Name s -> HST.Name s
tfHSEtoHSTName (HSE.Ident  srcS name) = HST.Ident srcS name
tfHSEtoHSTName (HSE.Symbol srcS name) = HST.Symbol srcS name

tfHSEtoHSTQOp :: HSE.QOp s -> HST.QOp s
tfHSEtoHSTQOp (HSE.QVarOp srcS qName) = HST.QVarOp srcS (tfHSEtoHSTQName qName)
tfHSEtoHSTQOp (HSE.QConOp srcS qName) = HST.QConOp srcS (tfHSEtoHSTQName qName)

tfHSEtoHSTSpecialCon :: HSE.SpecialCon s -> HST.SpecialCon s
tfHSEtoHSTSpecialCon (HSE.UnitCon srcS) = HST.UnitCon srcS
tfHSEtoHSTSpecialCon (HSE.ListCon srcS) = HST.ListCon srcS
tfHSEtoHSTSpecialCon (HSE.FunCon  srcS) = HST.FunCon srcS
tfHSEtoHSTSpecialCon (HSE.TupleCon srcS bxd n) =
  HST.TupleCon srcS (tfHSEtoHSTBoxed bxd) n
tfHSEtoHSTSpecialCon (HSE.Cons             srcS) = HST.Cons srcS
tfHSEtoHSTSpecialCon (HSE.UnboxedSingleCon srcS) = HST.UnboxedSingleCon srcS
tfHSEtoHSTSpecialCon (HSE.ExprHole         srcS) = HST.ExprHole srcS
