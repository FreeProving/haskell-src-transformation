module HST.Frontend.ToHSE where

import qualified Language.Haskell.Exts.Syntax  as HSE
import qualified Language.Haskell.Exts.SrcLoc  as Src

import qualified HST.Frontend.Syntax           as S

transformModule
  :: HSE.Module Src.SrcSpanInfo
  -> S.Module
       Src.SrcSpanInfo
       (HSE.Literal Src.SrcSpanInfo)
       (HSE.Type Src.SrcSpanInfo)
  -> HSE.Module Src.SrcSpanInfo
transformModule (HSE.Module srcS mmh pragmas impDecls oDecls) (S.Module aDecls)
  = HSE.Module
    srcS
    mmh
    pragmas
    impDecls
    (combineDecls oDecls (map transformDecl (filter isFun aDecls)))
 where
  isFun (S.FunBind _ _) = True
  isFun _               = False
  combineDecls (HSE.FunBind _ _ : oDecls') (aDecl : aDecls') =
    aDecl : combineDecls oDecls' aDecls'
  combineDecls (HSE.PatBind _ _ _ _ : oDecls') (aDecl : aDecls') =
    aDecl : combineDecls oDecls' aDecls'
  combineDecls (oDecl : oDecls') aDecls' = oDecl : combineDecls oDecls' aDecls'
  combineDecls []                aDecls' = aDecls'
transformModule _ _ = error "Unsupported Module type"


transformDecl
  :: S.Decl
       Src.SrcSpanInfo
       (HSE.Literal Src.SrcSpanInfo)
       (HSE.Type Src.SrcSpanInfo)
  -> HSE.Decl Src.SrcSpanInfo
transformDecl (S.DataDecl _ _) =
  error "Data type declarations should not be transformed back"
transformDecl (S.TypeSig s names typ) =
  HSE.TypeSig (transformSrcSpan s) (map transformName names) typ
transformDecl (S.FunBind s matches) =
  HSE.FunBind (transformSrcSpan s) (map transformMatch matches)

transformBinds
  :: S.Binds
       Src.SrcSpanInfo
       (HSE.Literal Src.SrcSpanInfo)
       (HSE.Type Src.SrcSpanInfo)
  -> HSE.Binds Src.SrcSpanInfo
transformBinds (S.BDecls s decls) =
  HSE.BDecls (transformSrcSpan s) (map transformDecl decls)

transformMatch
  :: S.Match
       Src.SrcSpanInfo
       (HSE.Literal Src.SrcSpanInfo)
       (HSE.Type Src.SrcSpanInfo)
  -> HSE.Match Src.SrcSpanInfo
transformMatch (S.Match s name pats rhs mBinds) = HSE.Match
  (transformSrcSpan s)
  (transformName name)
  (map transformPat pats)
  (transformRhs rhs)
  (fmap transformBinds mBinds)
transformMatch (S.InfixMatch s pat name pats rhs mBinds) = HSE.InfixMatch
  (transformSrcSpan s)
  (transformPat pat)
  (transformName name)
  (map transformPat pats)
  (transformRhs rhs)
  (fmap transformBinds mBinds)

transformRhs
  :: S.Rhs
       Src.SrcSpanInfo
       (HSE.Literal Src.SrcSpanInfo)
       (HSE.Type Src.SrcSpanInfo)
  -> HSE.Rhs Src.SrcSpanInfo
transformRhs (S.UnGuardedRhs s e) =
  HSE.UnGuardedRhs (transformSrcSpan s) (transformExp e)
transformRhs (S.GuardedRhss s grhss) =
  HSE.GuardedRhss (transformSrcSpan s) (map transformGuardedRhs grhss)

transformGuardedRhs
  :: S.GuardedRhs
       Src.SrcSpanInfo
       (HSE.Literal Src.SrcSpanInfo)
       (HSE.Type Src.SrcSpanInfo)
  -> HSE.GuardedRhs Src.SrcSpanInfo
transformGuardedRhs (S.GuardedRhs s ge e) = HSE.GuardedRhs
  (transformSrcSpan s)
  [HSE.Qualifier (transformSrcSpan (S.getSrcExp ge)) (transformExp ge)]
  (transformExp e)


transformBoxed :: S.Boxed -> HSE.Boxed
transformBoxed S.Boxed   = HSE.Boxed
transformBoxed S.Unboxed = HSE.Unboxed

transformExp
  :: S.Exp
       Src.SrcSpanInfo
       (HSE.Literal Src.SrcSpanInfo)
       (HSE.Type Src.SrcSpanInfo)
  -> HSE.Exp Src.SrcSpanInfo
transformExp (S.Var s qName) =
  HSE.Var (transformSrcSpan s) (transformQName qName)
transformExp (S.Con s qName) =
  HSE.Con (transformSrcSpan s) (transformQName qName)
transformExp (S.Lit s lit           ) = HSE.Lit (transformSrcSpan s) lit
transformExp (S.InfixApp s e1 qOp e2) = HSE.InfixApp (transformSrcSpan s)
                                                     (transformExp e1)
                                                     (transformQOp qOp)
                                                     (transformExp e2)
transformExp (S.App s e1 e2) =
  HSE.App (transformSrcSpan s) (transformExp e1) (transformExp e2)
transformExp (S.NegApp s e) = HSE.NegApp (transformSrcSpan s) (transformExp e)
transformExp (S.Lambda s pats e) =
  HSE.Lambda (transformSrcSpan s) (map transformPat pats) (transformExp e)
transformExp (S.Let s binds e) =
  HSE.Let (transformSrcSpan s) (transformBinds binds) (transformExp e)
transformExp (S.If s e1 e2 e3) = HSE.If (transformSrcSpan s)
                                        (transformExp e1)
                                        (transformExp e2)
                                        (transformExp e3)
transformExp (S.Case s e alts) =
  HSE.Case (transformSrcSpan s) (transformExp e) (map transformAlt alts)
transformExp (S.Tuple s bxd es) =
  HSE.Tuple (transformSrcSpan s) (transformBoxed bxd) (map transformExp es)
transformExp (S.List s es) =
  HSE.List (transformSrcSpan s) (map transformExp es)
transformExp (S.Paren s e) = HSE.Paren (transformSrcSpan s) (transformExp e)
transformExp (S.ExpTypeSig s e typ) =
  HSE.ExpTypeSig (transformSrcSpan s) (transformExp e) typ

transformAlt
  :: S.Alt
       Src.SrcSpanInfo
       (HSE.Literal Src.SrcSpanInfo)
       (HSE.Type Src.SrcSpanInfo)
  -> HSE.Alt Src.SrcSpanInfo
transformAlt (S.Alt s pat rhs mBinds) = HSE.Alt (transformSrcSpan s)
                                                (transformPat pat)
                                                (transformRhs rhs)
                                                (fmap transformBinds mBinds)

transformPat
  :: S.Pat Src.SrcSpanInfo (HSE.Literal Src.SrcSpanInfo)
  -> HSE.Pat Src.SrcSpanInfo
transformPat (S.PVar s name) =
  HSE.PVar (transformSrcSpan s) (transformName name)
transformPat (S.PInfixApp s pat1 qName pat2) = HSE.PInfixApp
  (transformSrcSpan s)
  (transformPat pat1)
  (transformQName qName)
  (transformPat pat2)
transformPat (S.PApp s qName pats) =
  HSE.PApp (transformSrcSpan s) (transformQName qName) (map transformPat pats)
transformPat (S.PTuple s bxd pats) =
  HSE.PTuple (transformSrcSpan s) (transformBoxed bxd) (map transformPat pats)
transformPat (S.PParen s pat) =
  HSE.PParen (transformSrcSpan s) (transformPat pat)
transformPat (S.PList s pats) =
  HSE.PList (transformSrcSpan s) (map transformPat pats)
transformPat (S.PWildCard s) = HSE.PWildCard (transformSrcSpan s)

transformSign :: S.Sign Src.SrcSpanInfo -> HSE.Sign Src.SrcSpanInfo
transformSign (S.Signless s) = HSE.Signless (transformSrcSpan s)
transformSign (S.Negative s) = HSE.Negative (transformSrcSpan s)

transformModuleName
  :: S.ModuleName Src.SrcSpanInfo -> HSE.ModuleName Src.SrcSpanInfo
transformModuleName (S.ModuleName s name) =
  HSE.ModuleName (transformSrcSpan s) name

transformQName :: S.QName Src.SrcSpanInfo -> HSE.QName Src.SrcSpanInfo
transformQName (S.Qual s modName name) = HSE.Qual
  (transformSrcSpan s)
  (transformModuleName modName)
  (transformName name)
transformQName (S.UnQual s name) =
  HSE.UnQual (transformSrcSpan s) (transformName name)
transformQName (S.Special s spCon) =
  HSE.Special (transformSrcSpan s) (transformSpecialCon spCon)

transformName :: S.Name Src.SrcSpanInfo -> HSE.Name Src.SrcSpanInfo
transformName (S.Ident  s name) = HSE.Ident (transformSrcSpan s) name
transformName (S.Symbol s name) = HSE.Symbol (transformSrcSpan s) name

transformQOp :: S.QOp Src.SrcSpanInfo -> HSE.QOp Src.SrcSpanInfo
transformQOp (S.QVarOp s qName) =
  HSE.QVarOp (transformSrcSpan s) (transformQName qName)
transformQOp (S.QConOp s qName) =
  HSE.QConOp (transformSrcSpan s) (transformQName qName)

transformSpecialCon
  :: S.SpecialCon Src.SrcSpanInfo -> HSE.SpecialCon Src.SrcSpanInfo
transformSpecialCon (S.UnitCon s) = HSE.UnitCon (transformSrcSpan s)
transformSpecialCon (S.ListCon s) = HSE.ListCon (transformSrcSpan s)
transformSpecialCon (S.FunCon  s) = HSE.FunCon (transformSrcSpan s)
transformSpecialCon (S.TupleCon s bxd n) =
  HSE.TupleCon (transformSrcSpan s) (transformBoxed bxd) n
transformSpecialCon (S.Cons s) = HSE.Cons (transformSrcSpan s)
transformSpecialCon (S.UnboxedSingleCon s) =
  HSE.UnboxedSingleCon (transformSrcSpan s)
transformSpecialCon (S.ExprHole s) = HSE.ExprHole (transformSrcSpan s)

transformSrcSpan :: S.SrcSpan Src.SrcSpanInfo -> Src.SrcSpanInfo
transformSrcSpan s = case s of
  S.SrcSpan srcSpan -> srcSpan
  S.NoSrcSpan       -> Src.noSrcSpan
