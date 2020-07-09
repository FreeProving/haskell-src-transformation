-- | This module contains functions transforming Haskell modules and other
--   constructs of the AST data structure of the "HST.Frontend.Syntax" module
--   into the corresponding constructs of the AST data structure of
--   @haskell-src-exts@.
--
--   Note that a construct of the AST data structure of HST can only be
--   transformed to the corresponding HSE construct if the former is
--   instantiated with the HSE types for source spans, literals and type
--   expressions.

module HST.Frontend.ToHSE where

import qualified Language.Haskell.Exts.Syntax  as HSE
import qualified Language.Haskell.Exts.SrcLoc  as Src

import qualified HST.Frontend.Syntax           as S
import           HST.Frontend.FromHSE           ( HSE )

-- | Transforms the @haskell-src-transformations@ representation of a Haskell
--   module into the @haskell-src-exts@ representation of a Haskell module by
--   enriching it with information from the original HSE module.
transformModule
  :: HSE.Module Src.SrcSpanInfo -> S.Module HSE -> HSE.Module Src.SrcSpanInfo
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

-- | Transforms an HST declaration into an HSE declaration.
transformDecl :: S.Decl HSE -> HSE.Decl Src.SrcSpanInfo
transformDecl (S.DataDecl _ _) =
  error "Data type declarations should not be transformed back"
transformDecl (S.TypeSig s names typ) =
  HSE.TypeSig (transformSrcSpan s) (map transformName names) typ
transformDecl (S.FunBind s matches) =
  HSE.FunBind (transformSrcSpan s) (map transformMatch matches)

-- | Transforms an HST binding group into an HSE binding group.
transformBinds :: S.Binds HSE -> HSE.Binds Src.SrcSpanInfo
transformBinds (S.BDecls s decls) =
  HSE.BDecls (transformSrcSpan s) (map transformDecl decls)

-- | Transforms an HST match into an HSE match.
transformMatch :: S.Match HSE -> HSE.Match Src.SrcSpanInfo
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

-- | Transforms an HST right hand side into an HSE right hand side.
transformRhs :: S.Rhs HSE -> HSE.Rhs Src.SrcSpanInfo
transformRhs (S.UnGuardedRhs s e) =
  HSE.UnGuardedRhs (transformSrcSpan s) (transformExp e)
transformRhs (S.GuardedRhss s grhss) =
  HSE.GuardedRhss (transformSrcSpan s) (map transformGuardedRhs grhss)

-- | Transforms an HST guarded right hand side into an HSE guarded right hand
--   side.
transformGuardedRhs :: S.GuardedRhs HSE -> HSE.GuardedRhs Src.SrcSpanInfo
transformGuardedRhs (S.GuardedRhs s ge e) = HSE.GuardedRhs
  (transformSrcSpan s)
  [HSE.Qualifier (transformSrcSpan (S.getSrcExp ge)) (transformExp ge)]
  (transformExp e)

-- | Transforms an HST boxed mark into an HSE boxed mark.
transformBoxed :: S.Boxed -> HSE.Boxed
transformBoxed S.Boxed   = HSE.Boxed
transformBoxed S.Unboxed = HSE.Unboxed

-- | Transforms an HST expression into an HSE expression.
transformExp :: S.Exp HSE -> HSE.Exp Src.SrcSpanInfo
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

-- | Transforms an HST case alternative into an HSE case alternative.
transformAlt :: S.Alt HSE -> HSE.Alt Src.SrcSpanInfo
transformAlt (S.Alt s pat rhs mBinds) = HSE.Alt (transformSrcSpan s)
                                                (transformPat pat)
                                                (transformRhs rhs)
                                                (fmap transformBinds mBinds)

-- | Transforms an HST pattern into an HSE pattern.
transformPat :: S.Pat HSE -> HSE.Pat Src.SrcSpanInfo
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

-- | Transforms an HST module name into an HSE module name.
transformModuleName :: S.ModuleName HSE -> HSE.ModuleName Src.SrcSpanInfo
transformModuleName (S.ModuleName s name) =
  HSE.ModuleName (transformSrcSpan s) name

-- | Transforms an HST qualified name into an HSE qualified name.
transformQName :: S.QName HSE -> HSE.QName Src.SrcSpanInfo
transformQName (S.Qual s modName name) = HSE.Qual
  (transformSrcSpan s)
  (transformModuleName modName)
  (transformName name)
transformQName (S.UnQual s name) =
  HSE.UnQual (transformSrcSpan s) (transformName name)
transformQName (S.Special s spCon) =
  HSE.Special (transformSrcSpan s) (transformSpecialCon spCon)

-- | Transforms an HST name into an HSE name.
transformName :: S.Name HSE -> HSE.Name Src.SrcSpanInfo
transformName (S.Ident  s name) = HSE.Ident (transformSrcSpan s) name
transformName (S.Symbol s name) = HSE.Symbol (transformSrcSpan s) name

-- | Transforms an HST qualified operator into an HSE qualified operator.
transformQOp :: S.QOp HSE -> HSE.QOp Src.SrcSpanInfo
transformQOp (S.QVarOp s qName) =
  HSE.QVarOp (transformSrcSpan s) (transformQName qName)
transformQOp (S.QConOp s qName) =
  HSE.QConOp (transformSrcSpan s) (transformQName qName)

-- | Transforms an HST special constructor into an HSE special constructor.
transformSpecialCon :: S.SpecialCon HSE -> HSE.SpecialCon Src.SrcSpanInfo
transformSpecialCon (S.UnitCon s) = HSE.UnitCon (transformSrcSpan s)
transformSpecialCon (S.ListCon s) = HSE.ListCon (transformSrcSpan s)
transformSpecialCon (S.FunCon  s) = HSE.FunCon (transformSrcSpan s)
transformSpecialCon (S.TupleCon s bxd n) =
  HSE.TupleCon (transformSrcSpan s) (transformBoxed bxd) n
transformSpecialCon (S.Cons s) = HSE.Cons (transformSrcSpan s)
transformSpecialCon (S.UnboxedSingleCon s) =
  HSE.UnboxedSingleCon (transformSrcSpan s)
transformSpecialCon (S.ExprHole s) = HSE.ExprHole (transformSrcSpan s)

-- | Unwraps the HST type for source spans into an HSE source span.
transformSrcSpan :: S.SrcSpan HSE -> Src.SrcSpanInfo
transformSrcSpan s = case s of
  S.SrcSpan srcSpan -> srcSpan
  S.NoSrcSpan       -> Src.noSrcSpan
