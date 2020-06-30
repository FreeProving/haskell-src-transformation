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
tfHSEtoHSTExp (HSE.Lit srcS lit  ) = (HST.Lit srcS lit)
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

tfHSTtoHSEModule
  :: HSE.Module s -> HST.Module s (HSE.Literal s) (HSE.Type s) -> HSE.Module s
tfHSTtoHSEModule (HSE.Module srcS mmh pragmas impDecls oDecls) (HST.Module aDecls)
  = HSE.Module srcS
               mmh
               pragmas
               impDecls
               (combineDecls oDecls (map tfHSTtoHSEDecl aDecls))
 where
  combineDecls ((HSE.FunBind _ _) : oDecls') (aDecl : aDecls') =
    aDecl : combineDecls oDecls' aDecls'
  combineDecls (oDecl : oDecls') aDecls' = oDecl : combineDecls oDecls' aDecls'
  combineDecls []                aDecls' = aDecls'
tfHSTtoHSEModule _ _ = error "Unsupported Module type"


tfHSTtoHSEDecl :: HST.Decl s (HSE.Literal s) (HSE.Type s) -> HSE.Decl s
tfHSTtoHSEDecl (HST.FunBind srcS matches) =
  HSE.FunBind srcS (map tfHSTtoHSEMatch matches)
tfHSTtoHSEDecl (HST.TypeSig srcS names typ) =
  HSE.TypeSig srcS (map tfHSTtoHSEName names) typ

tfHSTtoHSEBinds :: HST.Binds s (HSE.Literal s) (HSE.Type s) -> HSE.Binds s
tfHSTtoHSEBinds (HST.BDecls srcS decls) =
  HSE.BDecls srcS (map tfHSTtoHSEDecl decls)

tfHSTtoHSEMatch :: HST.Match s (HSE.Literal s) (HSE.Type s) -> HSE.Match s
tfHSTtoHSEMatch (HST.Match srcS name pats rhs mBinds) = HSE.Match
  srcS
  (tfHSTtoHSEName name)
  (map tfHSTtoHSEPat pats)
  (tfHSTtoHSERhs rhs)
  (fmap tfHSTtoHSEBinds mBinds)
tfHSTtoHSEMatch (HST.InfixMatch srcS pat name pats rhs mBinds) = HSE.InfixMatch
  srcS
  (tfHSTtoHSEPat pat)
  (tfHSTtoHSEName name)
  (map tfHSTtoHSEPat pats)
  (tfHSTtoHSERhs rhs)
  (fmap tfHSTtoHSEBinds mBinds)

tfHSTtoHSERhs :: HST.Rhs s (HSE.Literal s) (HSE.Type s) -> HSE.Rhs s
tfHSTtoHSERhs (HST.UnGuardedRhs srcS e) =
  HSE.UnGuardedRhs srcS (tfHSTtoHSEExp e)
tfHSTtoHSERhs (HST.GuardedRhss srcS grhss) =
  HSE.GuardedRhss srcS (map tfHSTtoHSEGuardedRhs grhss)

tfHSTtoHSEGuardedRhs
  :: HST.GuardedRhs s (HSE.Literal s) (HSE.Type s) -> HSE.GuardedRhs s
tfHSTtoHSEGuardedRhs (HST.GuardedRhs srcS stmts e) =
  HSE.GuardedRhs srcS (map tfHSTtoHSEStmt stmts) (tfHSTtoHSEExp e)

tfHSTtoHSEBoxed :: HST.Boxed -> HSE.Boxed
tfHSTtoHSEBoxed HST.Boxed   = HSE.Boxed
tfHSTtoHSEBoxed HST.Unboxed = HSE.Unboxed

tfHSTtoHSEExp :: HST.Exp s (HSE.Literal s) (HSE.Type s) -> HSE.Exp s
tfHSTtoHSEExp (HST.Var srcS qName) = HSE.Var srcS (tfHSTtoHSEQName qName)
tfHSTtoHSEExp (HST.Con srcS qName) = HSE.Con srcS (tfHSTtoHSEQName qName)
tfHSTtoHSEExp (HST.Lit srcS lit  ) = (HSE.Lit srcS lit)
tfHSTtoHSEExp (HST.InfixApp srcS e1 qOp e2) =
  HSE.InfixApp srcS (tfHSTtoHSEExp e1) (tfHSTtoHSEQOp qOp) (tfHSTtoHSEExp e2)
tfHSTtoHSEExp (HST.App srcS e1 e2) =
  HSE.App srcS (tfHSTtoHSEExp e1) (tfHSTtoHSEExp e2)
tfHSTtoHSEExp (HST.NegApp srcS e) = HSE.NegApp srcS (tfHSTtoHSEExp e)
tfHSTtoHSEExp (HST.Lambda srcS pats e) =
  HSE.Lambda srcS (map tfHSTtoHSEPat pats) (tfHSTtoHSEExp e)
tfHSTtoHSEExp (HST.Let srcS binds e) =
  HSE.Let srcS (tfHSTtoHSEBinds binds) (tfHSTtoHSEExp e)
tfHSTtoHSEExp (HST.If srcS e1 e2 e3) =
  HSE.If srcS (tfHSTtoHSEExp e1) (tfHSTtoHSEExp e2) (tfHSTtoHSEExp e3)
tfHSTtoHSEExp (HST.Case srcS e alts) =
  HSE.Case srcS (tfHSTtoHSEExp e) (map tfHSTtoHSEAlt alts)
tfHSTtoHSEExp (HST.Do srcS stmts) = HSE.Do srcS (map tfHSTtoHSEStmt stmts)
tfHSTtoHSEExp (HST.Tuple srcS bxd es) =
  HSE.Tuple srcS (tfHSTtoHSEBoxed bxd) (map tfHSTtoHSEExp es)
tfHSTtoHSEExp (HST.List     srcS es) = HSE.List srcS (map tfHSTtoHSEExp es)
tfHSTtoHSEExp (HST.Paren    srcS e ) = HSE.Paren srcS (tfHSTtoHSEExp e)
tfHSTtoHSEExp (HST.EnumFrom srcS e ) = HSE.EnumFrom srcS (tfHSTtoHSEExp e)
tfHSTtoHSEExp (HST.EnumFromTo srcS e1 e2) =
  HSE.EnumFromTo srcS (tfHSTtoHSEExp e1) (tfHSTtoHSEExp e2)
tfHSTtoHSEExp (HST.EnumFromThen srcS e1 e2) =
  HSE.EnumFromThen srcS (tfHSTtoHSEExp e1) (tfHSTtoHSEExp e2)
tfHSTtoHSEExp (HST.EnumFromThenTo srcS e1 e2 e3) = HSE.EnumFromThenTo
  srcS
  (tfHSTtoHSEExp e1)
  (tfHSTtoHSEExp e2)
  (tfHSTtoHSEExp e3)
tfHSTtoHSEExp (HST.ListComp srcS e qStmts) =
  HSE.ListComp srcS (tfHSTtoHSEExp e) (map tfHSTtoHSEQualStmt qStmts)
tfHSTtoHSEExp (HST.ExpTypeSig srcS e typ) =
  HSE.ExpTypeSig srcS (tfHSTtoHSEExp e) typ

tfHSTtoHSEStmt :: HST.Stmt s (HSE.Literal s) (HSE.Type s) -> HSE.Stmt s
tfHSTtoHSEStmt (HST.Generator srcS pat e) =
  HSE.Generator srcS (tfHSTtoHSEPat pat) (tfHSTtoHSEExp e)
tfHSTtoHSEStmt (HST.Qualifier srcS e) = HSE.Qualifier srcS (tfHSTtoHSEExp e)
tfHSTtoHSEStmt (HST.LetStmt srcS binds) =
  HSE.LetStmt srcS (tfHSTtoHSEBinds binds)
tfHSTtoHSEStmt (HST.RecStmt srcS stmts) =
  HSE.RecStmt srcS (map tfHSTtoHSEStmt stmts)

tfHSTtoHSEQualStmt
  :: HST.QualStmt s (HSE.Literal s) (HSE.Type s) -> HSE.QualStmt s
tfHSTtoHSEQualStmt (HST.QualStmt srcS stmt) =
  HSE.QualStmt srcS (tfHSTtoHSEStmt stmt)

tfHSTtoHSEAlt :: HST.Alt s (HSE.Literal s) (HSE.Type s) -> HSE.Alt s
tfHSTtoHSEAlt (HST.Alt srcS pat rhs mBinds) = HSE.Alt
  srcS
  (tfHSTtoHSEPat pat)
  (tfHSTtoHSERhs rhs)
  (fmap tfHSTtoHSEBinds mBinds)

tfHSTtoHSEPat :: HST.Pat s (HSE.Literal s) -> HSE.Pat s
tfHSTtoHSEPat (HST.PVar srcS name) = HSE.PVar srcS (tfHSTtoHSEName name)
tfHSTtoHSEPat (HST.PLit srcS sign lit) =
  HSE.PLit srcS (tfHSTtoHSESign sign) lit
tfHSTtoHSEPat (HST.PInfixApp srcS pat1 qName pat2) = HSE.PInfixApp
  srcS
  (tfHSTtoHSEPat pat1)
  (tfHSTtoHSEQName qName)
  (tfHSTtoHSEPat pat2)
tfHSTtoHSEPat (HST.PApp srcS qName pats) =
  HSE.PApp srcS (tfHSTtoHSEQName qName) (map tfHSTtoHSEPat pats)
tfHSTtoHSEPat (HST.PTuple srcS bxd pats) =
  HSE.PTuple srcS (tfHSTtoHSEBoxed bxd) (map tfHSTtoHSEPat pats)
tfHSTtoHSEPat (HST.PParen srcS pat ) = HSE.PParen srcS (tfHSTtoHSEPat pat)
tfHSTtoHSEPat (HST.PList  srcS pats) = HSE.PList srcS (map tfHSTtoHSEPat pats)
tfHSTtoHSEPat (HST.PWildCard srcS  ) = HSE.PWildCard srcS

tfHSTtoHSESign :: HST.Sign s -> HSE.Sign s
tfHSTtoHSESign (HST.Signless srcS) = HSE.Signless srcS
tfHSTtoHSESign (HST.Negative srcS) = HSE.Negative srcS

tfHSTtoHSEModuleName :: HST.ModuleName s -> HSE.ModuleName s
tfHSTtoHSEModuleName (HST.ModuleName srcS name) = HSE.ModuleName srcS name

tfHSTtoHSEQName :: HST.QName s -> HSE.QName s
tfHSTtoHSEQName (HST.Qual srcS modName name) =
  HSE.Qual srcS (tfHSTtoHSEModuleName modName) (tfHSTtoHSEName name)
tfHSTtoHSEQName (HST.UnQual srcS name) = HSE.UnQual srcS (tfHSTtoHSEName name)
tfHSTtoHSEQName (HST.Special srcS spCon) =
  HSE.Special srcS (tfHSTtoHSESpecialCon spCon)

tfHSTtoHSEName :: HST.Name s -> HSE.Name s
tfHSTtoHSEName (HST.Ident  srcS name) = HSE.Ident srcS name
tfHSTtoHSEName (HST.Symbol srcS name) = HSE.Symbol srcS name

tfHSTtoHSEQOp :: HST.QOp s -> HSE.QOp s
tfHSTtoHSEQOp (HST.QVarOp srcS qName) = HSE.QVarOp srcS (tfHSTtoHSEQName qName)
tfHSTtoHSEQOp (HST.QConOp srcS qName) = HSE.QConOp srcS (tfHSTtoHSEQName qName)

tfHSTtoHSESpecialCon :: HST.SpecialCon s -> HSE.SpecialCon s
tfHSTtoHSESpecialCon (HST.UnitCon srcS) = HSE.UnitCon srcS
tfHSTtoHSESpecialCon (HST.ListCon srcS) = HSE.ListCon srcS
tfHSTtoHSESpecialCon (HST.FunCon  srcS) = HSE.FunCon srcS
tfHSTtoHSESpecialCon (HST.TupleCon srcS bxd n) =
  HSE.TupleCon srcS (tfHSTtoHSEBoxed bxd) n
tfHSTtoHSESpecialCon (HST.Cons             srcS) = HSE.Cons srcS
tfHSTtoHSESpecialCon (HST.UnboxedSingleCon srcS) = HSE.UnboxedSingleCon srcS
tfHSTtoHSESpecialCon (HST.ExprHole         srcS) = HSE.ExprHole srcS
