-- | This module contains functions transforming Haskell modules and other
--   constructs of the AST data structure of the "HST.Frontend.Syntax" module
--   into the corresponding constructs of the AST data structure of
--   @haskell-src-exts@.
--
--   Note that a construct of the AST data structure of HST can only be
--   transformed to the corresponding HSE construct if the former is
--   instantiated with the HSE types for source spans, literals and type
--   expressions.
module HST.Frontend.HSE.To where

import qualified Language.Haskell.Exts   as HSE

import           HST.Frontend.HSE.Config
  ( HSE, OriginalModuleHead(originalModuleHead, originalModulePragmas,
                   originalModuleImports) )
import qualified HST.Frontend.Syntax     as S

-------------------------------------------------------------------------------
-- Modules                                                                   --
-------------------------------------------------------------------------------
-- | Transforms the @haskell-src-transformations@ representation of a Haskell
--   module into the @haskell-src-exts@ representation of a Haskell module.
--
--   The module head is restored from the original module head. The module
--   name field does not affect the name of the resulting module.
transformModule
  :: TransformSrcSpan srcSpan => S.Module (HSE srcSpan) -> HSE.Module srcSpan
transformModule (S.Module s origModuleHead _ _ decls) = HSE.Module
  (transformSrcSpan s) (originalModuleHead origModuleHead)
  (originalModulePragmas origModuleHead) (originalModuleImports origModuleHead)
  (map transformDecl decls)

-------------------------------------------------------------------------------
-- Declarations                                                              --
-------------------------------------------------------------------------------
-- | Transforms an HST declaration into an HSE declaration.
--
--   Only function declarations are actually transformed from the
--   intermediate representation to @HSE@. All other declarations
--   (including data type declarations) are restored from the
--   original declaration stored in the AST.
transformDecl
  :: TransformSrcSpan srcSpan => S.Decl (HSE srcSpan) -> HSE.Decl srcSpan
transformDecl (S.FunBind s matches)           = HSE.FunBind (transformSrcSpan s)
  (map transformMatch matches)
transformDecl (S.DataDecl _ originalDecl _ _) = originalDecl
transformDecl (S.OtherDecl _ originalDecl)    = originalDecl

-------------------------------------------------------------------------------
-- Function Declarations                                                     --
-------------------------------------------------------------------------------
-- | Transforms an HST binding group into an HSE binding group.
transformBinds
  :: TransformSrcSpan srcSpan => S.Binds (HSE srcSpan) -> HSE.Binds srcSpan
transformBinds (S.BDecls s decls) = HSE.BDecls (transformSrcSpan s)
  (map transformDecl decls)

-- | Transforms an HST match into an HSE match.
transformMatch
  :: TransformSrcSpan srcSpan => S.Match (HSE srcSpan) -> HSE.Match srcSpan
transformMatch (S.Match s name pats rhs mBinds)          = HSE.Match
  (transformSrcSpan s) (transformName name) (map transformPat pats)
  (transformRhs rhs) (fmap transformBinds mBinds)
transformMatch (S.InfixMatch s pat name pats rhs mBinds) = HSE.InfixMatch
  (transformSrcSpan s) (transformPat pat) (transformName name)
  (map transformPat pats) (transformRhs rhs) (fmap transformBinds mBinds)

-- | Transforms an HST right hand side into an HSE right hand side.
transformRhs
  :: TransformSrcSpan srcSpan => S.Rhs (HSE srcSpan) -> HSE.Rhs srcSpan
transformRhs (S.UnGuardedRhs s e)    = HSE.UnGuardedRhs (transformSrcSpan s)
  (transformExp e)
transformRhs (S.GuardedRhss s grhss) = HSE.GuardedRhss (transformSrcSpan s)
  (map transformGuardedRhs grhss)

-- | Transforms an HST guarded right hand side into an HSE guarded right hand
--   side.
transformGuardedRhs :: TransformSrcSpan srcSpan
                    => S.GuardedRhs (HSE srcSpan)
                    -> HSE.GuardedRhs srcSpan
transformGuardedRhs (S.GuardedRhs s ge e) = HSE.GuardedRhs (transformSrcSpan s)
  [HSE.Qualifier (transformSrcSpan (S.getSrcSpan ge)) (transformExp ge)]
  (transformExp e)

-------------------------------------------------------------------------------
-- Expressions                                                               --
-------------------------------------------------------------------------------
-- | Transforms an HST boxed mark into an HSE boxed mark.
transformBoxed :: S.Boxed -> HSE.Boxed
transformBoxed S.Boxed   = HSE.Boxed
transformBoxed S.Unboxed = HSE.Unboxed

-- | Transforms an HST expression into an HSE expression.
transformExp
  :: TransformSrcSpan srcSpan => S.Exp (HSE srcSpan) -> HSE.Exp srcSpan
transformExp (S.Var s qName)          = HSE.Var (transformSrcSpan s)
  (transformQName qName)
transformExp (S.Con s qName)          = HSE.Con (transformSrcSpan s)
  (transformQName qName)
transformExp (S.Lit s lit)            = HSE.Lit (transformSrcSpan s) lit
transformExp (S.InfixApp s e1 qOp e2) = HSE.InfixApp (transformSrcSpan s)
  (transformExp e1) (transformQOp qOp) (transformExp e2)
transformExp (S.App s e1 e2)          = HSE.App (transformSrcSpan s)
  (transformExp e1) (transformExp e2)
transformExp (S.NegApp s e)           = HSE.NegApp (transformSrcSpan s)
  (transformExp e)
transformExp (S.Lambda s pats e)      = HSE.Lambda (transformSrcSpan s)
  (map transformPat pats) (transformExp e)
transformExp (S.Let s binds e)        = HSE.Let (transformSrcSpan s)
  (transformBinds binds) (transformExp e)
transformExp (S.If s e1 e2 e3)        = HSE.If (transformSrcSpan s)
  (transformExp e1) (transformExp e2) (transformExp e3)
transformExp (S.Case s e alts)        = HSE.Case (transformSrcSpan s)
  (transformExp e) (map transformAlt alts)
transformExp (S.Tuple s bxd es)       = HSE.Tuple (transformSrcSpan s)
  (transformBoxed bxd) (map transformExp es)
transformExp (S.List s es)            = HSE.List (transformSrcSpan s)
  (map transformExp es)
transformExp (S.Paren s e)            = HSE.Paren (transformSrcSpan s)
  (transformExp e)
transformExp (S.ExpTypeSig s e typ)   = HSE.ExpTypeSig (transformSrcSpan s)
  (transformExp e) typ

-- | Transforms an HST case alternative into an HSE case alternative.
transformAlt
  :: TransformSrcSpan srcSpan => S.Alt (HSE srcSpan) -> HSE.Alt srcSpan
transformAlt (S.Alt s pat rhs mBinds) = HSE.Alt (transformSrcSpan s)
  (transformPat pat) (transformRhs rhs) (fmap transformBinds mBinds)

-------------------------------------------------------------------------------
-- Patterns                                                                  --
-------------------------------------------------------------------------------
-- | Transforms an HST pattern into an HSE pattern.
transformPat
  :: TransformSrcSpan srcSpan => S.Pat (HSE srcSpan) -> HSE.Pat srcSpan
transformPat (S.PVar s name)                 = HSE.PVar (transformSrcSpan s)
  (transformName name)
transformPat (S.PInfixApp s pat1 qName pat2) = HSE.PInfixApp
  (transformSrcSpan s) (transformPat pat1) (transformQName qName)
  (transformPat pat2)
transformPat (S.PApp s qName pats)           = HSE.PApp (transformSrcSpan s)
  (transformQName qName) (map transformPat pats)
transformPat (S.PTuple s bxd pats)           = HSE.PTuple (transformSrcSpan s)
  (transformBoxed bxd) (map transformPat pats)
transformPat (S.PParen s pat)                = HSE.PParen (transformSrcSpan s)
  (transformPat pat)
transformPat (S.PList s pats)                = HSE.PList (transformSrcSpan s)
  (map transformPat pats)
transformPat (S.PWildCard s)                 = HSE.PWildCard
  (transformSrcSpan s)

-------------------------------------------------------------------------------
-- Names                                                                     --
-------------------------------------------------------------------------------
-- | Transforms an HST module name into an HSE module name.
transformModuleName :: TransformSrcSpan srcSpan
                    => S.ModuleName (HSE srcSpan)
                    -> HSE.ModuleName srcSpan
transformModuleName (S.ModuleName s name) = HSE.ModuleName (transformSrcSpan s)
  name

-- | Transforms an HST qualified name into an HSE qualified name.
transformQName
  :: TransformSrcSpan srcSpan => S.QName (HSE srcSpan) -> HSE.QName srcSpan
transformQName (S.Qual s modName name) = HSE.Qual (transformSrcSpan s)
  (transformModuleName modName) (transformName name)
transformQName (S.UnQual s name)       = HSE.UnQual (transformSrcSpan s)
  (transformName name)
transformQName (S.Special s spCon)     = HSE.Special (transformSrcSpan s)
  (transformSpecialCon spCon)

-- | Transforms an HST name into an HSE name.
transformName
  :: TransformSrcSpan srcSpan => S.Name (HSE srcSpan) -> HSE.Name srcSpan
transformName (S.Ident s name)  = HSE.Ident (transformSrcSpan s) name
transformName (S.Symbol s name) = HSE.Symbol (transformSrcSpan s) name

-- | Transforms an HST qualified operator into an HSE qualified operator.
transformQOp
  :: TransformSrcSpan srcSpan => S.QOp (HSE srcSpan) -> HSE.QOp srcSpan
transformQOp (S.QVarOp s qName) = HSE.QVarOp (transformSrcSpan s)
  (transformQName qName)
transformQOp (S.QConOp s qName) = HSE.QConOp (transformSrcSpan s)
  (transformQName qName)

-- | Transforms an HST special constructor into an HSE special constructor.
transformSpecialCon :: TransformSrcSpan srcSpan
                    => S.SpecialCon (HSE srcSpan)
                    -> HSE.SpecialCon srcSpan
transformSpecialCon (S.UnitCon s)          = HSE.UnitCon (transformSrcSpan s)
transformSpecialCon (S.UnboxedSingleCon s) = HSE.UnboxedSingleCon
  (transformSrcSpan s)
transformSpecialCon (S.TupleCon s bxd n)   = HSE.TupleCon (transformSrcSpan s)
  (transformBoxed bxd) n
transformSpecialCon (S.NilCon s)           = HSE.ListCon (transformSrcSpan s)
transformSpecialCon (S.ConsCon s)          = HSE.Cons (transformSrcSpan s)
transformSpecialCon (S.ExprHole s)         = HSE.ExprHole (transformSrcSpan s)

-------------------------------------------------------------------------------
-- Source Spans                                                              --
-------------------------------------------------------------------------------
-- | Type class for annotation types that can be used as a source span of
--   the @haskell-src-exts@ AST.
class TransformSrcSpan srcSpan where
  transformSrcSpan :: S.SrcSpan (HSE srcSpan) -> srcSpan

-- | Unwraps the HST type for source spans into an HSE source span.
instance TransformSrcSpan HSE.SrcSpanInfo where
  transformSrcSpan (S.SrcSpan originalSrcSpan _) = originalSrcSpan
  transformSrcSpan S.NoSrcSpan                   = HSE.noSrcSpan
