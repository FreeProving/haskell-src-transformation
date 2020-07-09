{-# LANGUAGE TypeFamilies #-}

module HST.Frontend.FromHSE where

import           Data.Maybe                     ( fromMaybe
                                                , mapMaybe
                                                )

import qualified Language.Haskell.Exts.Syntax  as HSE
import qualified Language.Haskell.Exts.SrcLoc  as Src

import qualified HST.Frontend.Syntax           as S

data HSE
type instance S.SrcSpanType HSE = Src.SrcSpanInfo
type instance S.Literal HSE = HSE.Literal Src.SrcSpanInfo
type instance S.TypeExp HSE = HSE.Type Src.SrcSpanInfo

instance S.EqAST HSE
instance S.ShowAST HSE

transformModule :: HSE.Module Src.SrcSpanInfo -> S.Module HSE
transformModule (HSE.Module _ _ _ _ decls) =
  S.Module (mapMaybe transformDecl decls)
transformModule _ = error "Unsupported Module type"

transformDecl :: HSE.Decl Src.SrcSpanInfo -> Maybe (S.Decl HSE)
transformDecl (HSE.DataDecl _ (HSE.DataType _) _ dHead qcds _) =
  Just (S.DataDecl (transformDeclHead dHead) (map transformQualConDecl qcds))
transformDecl (HSE.TypeSig s names typ) =
  Just (S.TypeSig (transformSrcSpan s) (map transformName names) typ)
transformDecl (HSE.FunBind s matches) =
  Just (S.FunBind (transformSrcSpan s) (map transformMatch matches))
transformDecl (HSE.PatBind s (HSE.PVar _ name) rhs mBinds) = Just
  (S.FunBind (transformSrcSpan s)
             [transformMatch (HSE.Match s name [] rhs mBinds)]
  )
transformDecl _ = Nothing

transformDeclHead :: HSE.DeclHead Src.SrcSpanInfo -> S.DeclHead HSE
transformDeclHead (HSE.DHead _ dName    ) = S.DHead (transformName dName)
transformDeclHead (HSE.DHInfix _ _ dName) = S.DHInfix (transformName dName)
transformDeclHead (HSE.DHParen _ dHead  ) = S.DHParen (transformDeclHead dHead)
transformDeclHead (HSE.DHApp _ dHead _  ) = S.DHApp (transformDeclHead dHead)

transformQualConDecl :: HSE.QualConDecl Src.SrcSpanInfo -> S.ConDecl HSE
transformQualConDecl (HSE.QualConDecl _ _ _ conDecl) = transformConDecl conDecl

transformConDecl :: HSE.ConDecl Src.SrcSpanInfo -> S.ConDecl HSE
transformConDecl (HSE.ConDecl _ cName types) =
  S.ConDecl (transformName cName) types
transformConDecl (HSE.InfixConDecl _ t1 cName t2) =
  S.InfixConDecl t1 (transformName cName) t2
transformConDecl (HSE.RecDecl _ cName _) = S.RecDecl (transformName cName)

transformBinds :: HSE.Binds Src.SrcSpanInfo -> S.Binds HSE
transformBinds (HSE.BDecls s decls) = S.BDecls
  (transformSrcSpan s)
  (map (fromMaybe (error "Unsupported declaration") . transformDecl) decls)
transformBinds _ = error "Implicit bindings are not supported"

transformMatch :: HSE.Match Src.SrcSpanInfo -> S.Match HSE
transformMatch (HSE.Match s name pats rhs mBinds) = S.Match
  (transformSrcSpan s)
  (transformName name)
  (map transformPat pats)
  (transformRhs rhs)
  (fmap transformBinds mBinds)
transformMatch (HSE.InfixMatch s pat name pats rhs mBinds) = S.InfixMatch
  (transformSrcSpan s)
  (transformPat pat)
  (transformName name)
  (map transformPat pats)
  (transformRhs rhs)
  (fmap transformBinds mBinds)

transformRhs :: HSE.Rhs Src.SrcSpanInfo -> S.Rhs HSE
transformRhs (HSE.UnGuardedRhs s e) =
  S.UnGuardedRhs (transformSrcSpan s) (transformExp e)
transformRhs (HSE.GuardedRhss s grhss) =
  S.GuardedRhss (transformSrcSpan s) (map transformGuardedRhs grhss)

transformGuardedRhs :: HSE.GuardedRhs Src.SrcSpanInfo -> S.GuardedRhs HSE
transformGuardedRhs (HSE.GuardedRhs s [HSE.Qualifier _ ge] e) =
  S.GuardedRhs (transformSrcSpan s) (transformExp ge) (transformExp e)
transformGuardedRhs _ =
  error "Only boolean expressions are supported as statements"

transformBoxed :: HSE.Boxed -> S.Boxed
transformBoxed HSE.Boxed   = S.Boxed
transformBoxed HSE.Unboxed = S.Unboxed

transformExp :: HSE.Exp Src.SrcSpanInfo -> S.Exp HSE
transformExp (HSE.Var s qName) =
  S.Var (transformSrcSpan s) (transformQName qName)
transformExp (HSE.Con s qName) =
  S.Con (transformSrcSpan s) (transformQName qName)
transformExp (HSE.Lit s lit           ) = S.Lit (transformSrcSpan s) lit
transformExp (HSE.InfixApp s e1 qOp e2) = S.InfixApp (transformSrcSpan s)
                                                     (transformExp e1)
                                                     (transformQOp qOp)
                                                     (transformExp e2)
transformExp (HSE.App s e1 e2) =
  S.App (transformSrcSpan s) (transformExp e1) (transformExp e2)
transformExp (HSE.NegApp s e) = S.NegApp (transformSrcSpan s) (transformExp e)
transformExp (HSE.Lambda s pats e) =
  S.Lambda (transformSrcSpan s) (map transformPat pats) (transformExp e)
transformExp (HSE.Let s binds e) =
  S.Let (transformSrcSpan s) (transformBinds binds) (transformExp e)
transformExp (HSE.If s e1 e2 e3) = S.If (transformSrcSpan s)
                                        (transformExp e1)
                                        (transformExp e2)
                                        (transformExp e3)
transformExp (HSE.Case s e alts) =
  S.Case (transformSrcSpan s) (transformExp e) (map transformAlt alts)
transformExp (HSE.Tuple s bxd es) =
  S.Tuple (transformSrcSpan s) (transformBoxed bxd) (map transformExp es)
transformExp (HSE.List s es) =
  S.List (transformSrcSpan s) (map transformExp es)
transformExp (HSE.Paren s e) = S.Paren (transformSrcSpan s) (transformExp e)
transformExp (HSE.ExpTypeSig s e typ) =
  S.ExpTypeSig (transformSrcSpan s) (transformExp e) typ
transformExp _ = error "Unsupported Expression type"

transformAlt :: HSE.Alt Src.SrcSpanInfo -> S.Alt HSE
transformAlt (HSE.Alt s pat rhs mBinds) = S.Alt (transformSrcSpan s)
                                                (transformPat pat)
                                                (transformRhs rhs)
                                                (fmap transformBinds mBinds)

transformPat :: HSE.Pat Src.SrcSpanInfo -> S.Pat HSE
transformPat (HSE.PVar s name) =
  S.PVar (transformSrcSpan s) (transformName name)
transformPat (HSE.PInfixApp s pat1 qName pat2) = S.PInfixApp
  (transformSrcSpan s)
  (transformPat pat1)
  (transformQName qName)
  (transformPat pat2)
transformPat (HSE.PApp s qName pats) =
  S.PApp (transformSrcSpan s) (transformQName qName) (map transformPat pats)
transformPat (HSE.PTuple s bxd pats) =
  S.PTuple (transformSrcSpan s) (transformBoxed bxd) (map transformPat pats)
transformPat (HSE.PParen s pat) =
  S.PParen (transformSrcSpan s) (transformPat pat)
transformPat (HSE.PList s pats) =
  S.PList (transformSrcSpan s) (map transformPat pats)
transformPat (HSE.PWildCard s) = S.PWildCard (transformSrcSpan s)
transformPat _                 = error "Unsupported Pattern type"

transformModuleName :: HSE.ModuleName Src.SrcSpanInfo -> S.ModuleName HSE
transformModuleName (HSE.ModuleName s name) =
  S.ModuleName (transformSrcSpan s) name

transformQName :: HSE.QName Src.SrcSpanInfo -> S.QName HSE
transformQName (HSE.Qual s modName name) =
  S.Qual (transformSrcSpan s) (transformModuleName modName) (transformName name)
transformQName (HSE.UnQual s name) =
  S.UnQual (transformSrcSpan s) (transformName name)
transformQName (HSE.Special s spCon) =
  S.Special (transformSrcSpan s) (transformSpecialCon spCon)

transformName :: HSE.Name Src.SrcSpanInfo -> S.Name HSE
transformName (HSE.Ident  s name) = S.Ident (transformSrcSpan s) name
transformName (HSE.Symbol s name) = S.Symbol (transformSrcSpan s) name

transformQOp :: HSE.QOp Src.SrcSpanInfo -> S.QOp HSE
transformQOp (HSE.QVarOp s qName) =
  S.QVarOp (transformSrcSpan s) (transformQName qName)
transformQOp (HSE.QConOp s qName) =
  S.QConOp (transformSrcSpan s) (transformQName qName)

transformSpecialCon :: HSE.SpecialCon Src.SrcSpanInfo -> S.SpecialCon HSE
transformSpecialCon (HSE.UnitCon s) = S.UnitCon (transformSrcSpan s)
transformSpecialCon (HSE.ListCon s) = S.ListCon (transformSrcSpan s)
transformSpecialCon (HSE.FunCon  s) = S.FunCon (transformSrcSpan s)
transformSpecialCon (HSE.TupleCon s bxd n) =
  S.TupleCon (transformSrcSpan s) (transformBoxed bxd) n
transformSpecialCon (HSE.Cons s) = S.Cons (transformSrcSpan s)
transformSpecialCon (HSE.UnboxedSingleCon s) =
  S.UnboxedSingleCon (transformSrcSpan s)
transformSpecialCon (HSE.ExprHole s) = S.ExprHole (transformSrcSpan s)

transformSrcSpan :: Src.SrcSpanInfo -> S.SrcSpan HSE
transformSrcSpan = S.SrcSpan
