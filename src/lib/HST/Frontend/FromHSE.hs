{-# LANGUAGE TypeFamilies #-}

-- | This module contains functions transforming Haskell modules and other
--   constructs of the AST data structure of @haskell-src-exts@ into the
--   corresponding constructs of the AST data structure in the
--   "HST.Frontend.Syntax" module.

module HST.Frontend.FromHSE where

import           Data.Maybe                     ( fromMaybe
                                                , mapMaybe
                                                )

import qualified Language.Haskell.Exts         as HSE

import qualified HST.Frontend.Syntax           as S

-- | Type representing the AST data structure of @haskell-src-exts@.
--
--   Instantiates the type families for source spans, literals and type
--   expressions with the concrete types from @haskell-src-exts@. Also adds
--   instances for 'S.EqAST' and 'S.ShowAST' to allow the usage of @==@ and
--   @show@.
data HSE
type instance S.SrcSpanType HSE = HSE.SrcSpanInfo
type instance S.Literal HSE = HSE.Literal HSE.SrcSpanInfo
type instance S.TypeExp HSE = HSE.Type HSE.SrcSpanInfo

instance S.EqAST HSE
instance S.ShowAST HSE

-- | Transforms the @haskell-src-exts@ representation of a Haskell module into
--   the @haskell-src-transformations@ representation of a Haskell module.
transformModule :: HSE.Module HSE.SrcSpanInfo -> S.Module HSE
transformModule (HSE.Module _ moduleHead _ _ decls) = S.Module
  (fmap getModuleName moduleHead)
  (mapMaybe transformDecl decls)
 where
  getModuleName (HSE.ModuleHead _ name _ _) = getModuleName' name
  getModuleName' (HSE.ModuleName _ name) = S.ModuleName S.NoSrcSpan name
transformModule _ = error "Unsupported Module type"

-- | Transforms an HSE declaration into an HST declaration.
--
--   Unlike the other transforming functions, the result is wrapped inside
--   the @Maybe@ type, so instead of an error, @Nothing@ is returned if the
--   HSE declaration cannot be transformed.
transformDecl :: HSE.Decl HSE.SrcSpanInfo -> Maybe (S.Decl HSE)
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

-- | Transforms an HSE declaration head into an HST declaration head.
transformDeclHead :: HSE.DeclHead HSE.SrcSpanInfo -> S.Name HSE
transformDeclHead (HSE.DHead _ dName    ) = transformName dName
transformDeclHead (HSE.DHInfix _ _ dName) = transformName dName
transformDeclHead (HSE.DHParen _ dHead  ) = transformDeclHead dHead
transformDeclHead (HSE.DHApp _ dHead _  ) = transformDeclHead dHead

-- | Transforms an HSE binding group into an HST binding group.
transformBinds :: HSE.Binds HSE.SrcSpanInfo -> S.Binds HSE
transformBinds (HSE.BDecls s decls) = S.BDecls
  (transformSrcSpan s)
  (map (fromMaybe (error "Unsupported declaration") . transformDecl) decls)
transformBinds _ = error "Implicit bindings are not supported"

-- | Transforms an HSE qualified constructor declaration into an HST
--   constructor declaration.
transformQualConDecl :: HSE.QualConDecl HSE.SrcSpanInfo -> S.ConDecl HSE
transformQualConDecl (HSE.QualConDecl _ _ _ conDecl) = transformConDecl conDecl

-- | Transforms an HSE constructor declaration into an HST constructor
--   declaration.
transformConDecl :: HSE.ConDecl HSE.SrcSpanInfo -> S.ConDecl HSE
transformConDecl (HSE.ConDecl _ cName types) =
  S.ConDecl (transformName cName) types
transformConDecl (HSE.InfixConDecl _ t1 cName t2) =
  S.InfixConDecl t1 (transformName cName) t2
transformConDecl (HSE.RecDecl _ _ _) =
  error "transformConDecl: record notation is not supported"

-- | Transforms an HSE match into an HST match.
transformMatch :: HSE.Match HSE.SrcSpanInfo -> S.Match HSE
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

-- | Transforms an HSE right hand side into an HST right hand side.
transformRhs :: HSE.Rhs HSE.SrcSpanInfo -> S.Rhs HSE
transformRhs (HSE.UnGuardedRhs s e) =
  S.UnGuardedRhs (transformSrcSpan s) (transformExp e)
transformRhs (HSE.GuardedRhss s grhss) =
  S.GuardedRhss (transformSrcSpan s) (map transformGuardedRhs grhss)

-- | Transforms an HSE guarded right hand side into an HST guarded right hand
--   side.
transformGuardedRhs :: HSE.GuardedRhs HSE.SrcSpanInfo -> S.GuardedRhs HSE
transformGuardedRhs (HSE.GuardedRhs s [HSE.Qualifier _ ge] e) =
  S.GuardedRhs (transformSrcSpan s) (transformExp ge) (transformExp e)
transformGuardedRhs _ =
  error "Only boolean expressions are supported as statements"

-- | Transforms an HSE boxed mark into an HST boxed mark.
transformBoxed :: HSE.Boxed -> S.Boxed
transformBoxed HSE.Boxed   = S.Boxed
transformBoxed HSE.Unboxed = S.Unboxed

-- | Transforms an HSE expression into an HST expression.
transformExp :: HSE.Exp HSE.SrcSpanInfo -> S.Exp HSE
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

-- | Transforms an HSE case alternative into an HST case alternative.
transformAlt :: HSE.Alt HSE.SrcSpanInfo -> S.Alt HSE
transformAlt (HSE.Alt s pat rhs mBinds) = S.Alt (transformSrcSpan s)
                                                (transformPat pat)
                                                (transformRhs rhs)
                                                (fmap transformBinds mBinds)

-- | Transforms an HSE pattern into an HST pattern.
transformPat :: HSE.Pat HSE.SrcSpanInfo -> S.Pat HSE
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

-- | Transforms an HSE module name into an HST module name.
transformModuleName :: HSE.ModuleName HSE.SrcSpanInfo -> S.ModuleName HSE
transformModuleName (HSE.ModuleName s name) =
  S.ModuleName (transformSrcSpan s) name

-- | Transforms an HSE qualified name into an HST qualified name.
transformQName :: HSE.QName HSE.SrcSpanInfo -> S.QName HSE
transformQName (HSE.Qual s modName name) =
  S.Qual (transformSrcSpan s) (transformModuleName modName) (transformName name)
transformQName (HSE.UnQual s name) =
  S.UnQual (transformSrcSpan s) (transformName name)
transformQName (HSE.Special s spCon) =
  S.Special (transformSrcSpan s) (transformSpecialCon spCon)

-- | Transforms an HSE name into an HST name.
transformName :: HSE.Name HSE.SrcSpanInfo -> S.Name HSE
transformName (HSE.Ident  s name) = S.Ident (transformSrcSpan s) name
transformName (HSE.Symbol s name) = S.Symbol (transformSrcSpan s) name

-- | Transforms an HSE qualified operator into an HST qualified operator.
transformQOp :: HSE.QOp HSE.SrcSpanInfo -> S.QOp HSE
transformQOp (HSE.QVarOp s qName) =
  S.QVarOp (transformSrcSpan s) (transformQName qName)
transformQOp (HSE.QConOp s qName) =
  S.QConOp (transformSrcSpan s) (transformQName qName)

-- | Transforms an HSE special constructor into an HST special constructor.
transformSpecialCon :: HSE.SpecialCon HSE.SrcSpanInfo -> S.SpecialCon HSE
transformSpecialCon (HSE.UnitCon s) = S.UnitCon (transformSrcSpan s)
transformSpecialCon (HSE.ListCon s) = S.ListCon (transformSrcSpan s)
transformSpecialCon (HSE.FunCon  s) = S.FunCon (transformSrcSpan s)
transformSpecialCon (HSE.TupleCon s bxd n) =
  S.TupleCon (transformSrcSpan s) (transformBoxed bxd) n
transformSpecialCon (HSE.Cons s) = S.Cons (transformSrcSpan s)
transformSpecialCon (HSE.UnboxedSingleCon s) =
  S.UnboxedSingleCon (transformSrcSpan s)
transformSpecialCon (HSE.ExprHole s) = S.ExprHole (transformSrcSpan s)

-- | Wraps an HSE source span into the HST type for source spans.
transformSrcSpan :: HSE.SrcSpanInfo -> S.SrcSpan HSE
transformSrcSpan = S.SrcSpan
