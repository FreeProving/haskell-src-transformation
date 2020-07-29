{-# LANGUAGE PackageImports #-}

-- | This module contains functions transforming Haskell modules and other
--   constructs of the AST data structure of the "HST.Frontend.Syntax" module
--   into the corresponding constructs of the AST data structure of
--   @ghc-lib-parser@.
--
--   Note that a construct of the AST data structure of HST can only be
--   transformed to the corresponding GHC construct if the former is
--   instantiated with the GHC types for source spans, literals and type
--   expressions.

module HST.Frontend.GHC.To where

import qualified "ghc-lib-parser" GHC.Hs       as GHC
import qualified "ghc-lib-parser" SrcLoc       as GHC
import qualified "ghc-lib-parser" RdrName      as GHC
import qualified "ghc-lib-parser" TcEvidence   as GHC
import qualified "ghc-lib-parser" BasicTypes   as GHC
import qualified "ghc-lib-parser" Bag          as GHC
import qualified "ghc-lib-parser" OccName      as GHC
import qualified "ghc-lib-parser" TysWiredIn   as GHC
import qualified "ghc-lib-parser" Module       as GHC
import qualified "ghc-lib-parser" Name         as GHC
import qualified "ghc-lib-parser" TyCon        as GHC
import qualified "ghc-lib-parser" Type         as GHC

import qualified HST.Frontend.Syntax           as S
import           HST.Frontend.GHC.Config        ( GHC
                                                , LitWrapper(Lit, OverLit)
                                                , TypeWrapper(SigType)
                                                , OriginalModuleHead
                                                  ( originalModuleName
                                                  , originalModuleExports
                                                  , originalModuleImports
                                                  , originalModuleDeprecMessage
                                                  , originalModuleHaddockModHeader
                                                  )
                                                , DeclWrapper(Decl)
                                                )

-------------------------------------------------------------------------------
-- Modules                                                                   --
-------------------------------------------------------------------------------

-- | Transforms the @haskell-src-transformations@ representation of a Haskell
--   module into the @ghc-lib-parser@ representation of a Haskell module.
--
--   The module head is restored from the original module head. The module
--   name field does not affect the name of the resulting module.
transformModule :: S.Module GHC -> GHC.Located (GHC.HsModule GHC.GhcPs)
transformModule (S.Module s omh _ decls) = GHC.L
  (transformSrcSpan s)
  GHC.HsModule { GHC.hsmodName             = originalModuleName omh
               , GHC.hsmodExports          = originalModuleExports omh
               , GHC.hsmodImports          = originalModuleImports omh
               , GHC.hsmodDecls            = map transformDecl decls
               , GHC.hsmodDeprecMessage    = originalModuleDeprecMessage omh
               , GHC.hsmodHaddockModHeader = originalModuleHaddockModHeader omh
               }

-------------------------------------------------------------------------------
-- Declarations                                                              --
-------------------------------------------------------------------------------

-- | Transforms an HST declaration into an GHC located declaration.
transformDecl :: S.Decl GHC -> GHC.LHsDecl GHC.GhcPs
transformDecl (S.DataDecl _ (Decl oDecl) _ _) = oDecl
transformDecl (S.FunBind s matches) =
  let s' = transformSrcSpan s
  in  GHC.L
        s'
        (GHC.ValD
          GHC.NoExtField
          GHC.FunBind
            { GHC.fun_ext     = GHC.NoExtField
            , GHC.fun_id = transformName GHC.varName (getMatchesName matches)
            , GHC.fun_matches = transformMatches Function s' matches
            , GHC.fun_co_fn   = GHC.WpHole
            , GHC.fun_tick    = []
            }
        )
 where
  getMatchesName :: [S.Match GHC] -> S.Name GHC
  getMatchesName (S.Match _ name _ _ _ : _) = name
  getMatchesName (S.InfixMatch _ _ name _ _ _ : _) = name
  getMatchesName _ = error "Empty match group"
transformDecl (S.OtherDecl _ (Decl oDecl)) = oDecl

-------------------------------------------------------------------------------
-- Function Declarations                                                     --
-------------------------------------------------------------------------------

-- | Transforms an HST binding group into a GHC located binding group.
transformMaybeBinds :: Maybe (S.Binds GHC) -> GHC.LHsLocalBinds GHC.GhcPs
transformMaybeBinds Nothing =
  GHC.L GHC.noSrcSpan (GHC.EmptyLocalBinds GHC.NoExtField)
transformMaybeBinds (Just (S.BDecls s decls)) =
  let (funBinds, sigs) = splitBDecls (map transformDecl decls)
  in  GHC.L
        (transformSrcSpan s)
        (GHC.HsValBinds
          GHC.NoExtField
          (GHC.ValBinds GHC.NoExtField (GHC.listToBag funBinds) sigs)
        )
 where
  splitBDecls
    :: [GHC.LHsDecl GHC.GhcPs]
    -> ([GHC.LHsBindLR GHC.GhcPs GHC.GhcPs], [GHC.LSig GHC.GhcPs])
  splitBDecls [] = ([], [])
  splitBDecls (decl : decls') =
    let (funBinds', sigs') = splitBDecls decls'
    in  case decl of
          GHC.L s' (GHC.ValD _ fb@GHC.FunBind{}) ->
            (GHC.L s' fb : funBinds', sigs')
          GHC.L s' (GHC.SigD _ sig) -> (funBinds', GHC.L s' sig : sigs')
          _ -> error "Unexpected declaration in bindings"

-- | Type for the contexts where a match or match group can occur in the GHC
--   AST data structure.
data MatchContext = Function | LambdaExp | CaseAlt
  deriving (Eq, Show)

-- | Transforms a match context, a GHC source span of the matches and a list of
--   HST matches with into a GHC match group.
transformMatches
  :: MatchContext
  -> GHC.SrcSpan
  -> [S.Match GHC]
  -> GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
transformMatches ctxt s matches = GHC.MG
  { GHC.mg_ext    = GHC.NoExtField
  , GHC.mg_alts   = GHC.L s (map (transformMatch ctxt) matches)
  , GHC.mg_origin = GHC.FromSource
  }

-- | Transforms an HST match with a match context into a GHC located match.
transformMatch
  :: MatchContext -> S.Match GHC -> GHC.LMatch GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
transformMatch ctxt match =
  let (s, name, pats, rhs, mBinds, fixity) = case match of
        S.Match s' name' pats' rhs' mBinds' ->
          (s', name', pats', rhs', mBinds', GHC.Prefix)
        S.InfixMatch s' pat name' pats' rhs' mBinds' ->
          (s', name', pat : pats', rhs', mBinds', GHC.Infix)
      ctxt' = case ctxt of
        Function -> GHC.FunRhs { GHC.mc_fun = transformName GHC.varName name
                               , GHC.mc_fixity = fixity
                               , GHC.mc_strictness = GHC.NoSrcStrict
                               }
        LambdaExp -> GHC.LambdaExpr
        CaseAlt   -> GHC.CaseAlt
  in  GHC.L
        (transformSrcSpan s)
        GHC.Match { GHC.m_ext   = GHC.NoExtField
                  , GHC.m_ctxt  = ctxt'
                  , GHC.m_pats  = map transformPat pats
                  , GHC.m_grhss = transformRhs rhs mBinds
                  }

-- | Transforms an HST right-hand side and binding group into GHC guarded
--   right-hand sides.
transformRhs
  :: S.Rhs GHC
  -> Maybe (S.Binds GHC)
  -> GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
transformRhs rhs mBinds =
  let grhss = case rhs of
        S.UnGuardedRhs s e ->
          [ GHC.L (transformSrcSpan s)
                  (GHC.GRHS GHC.NoExtField [] (transformExp e))
          ]
        S.GuardedRhss _ grhss' -> map transformGuardedRhs grhss'
  in  GHC.GRHSs { GHC.grhssExt        = GHC.NoExtField
                , GHC.grhssGRHSs      = grhss
                , GHC.grhssLocalBinds = transformMaybeBinds mBinds
                }

-- | Transforms an HST guarded right-hand side into a GHC located guarded
--   right-hand side.
transformGuardedRhs
  :: S.GuardedRhs GHC -> GHC.LGRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
transformGuardedRhs (S.GuardedRhs s ge be) = GHC.L
  (transformSrcSpan s)
  (GHC.GRHS
    GHC.NoExtField
    [ GHC.L
        (transformSrcSpan (S.getSrcSpan ge))
        (GHC.BodyStmt GHC.NoExtField
                      (transformExp ge)
                      GHC.noSyntaxExpr
                      GHC.noSyntaxExpr
        )
    ]
    (transformExp be)
  )

-------------------------------------------------------------------------------
-- Expressions                                                               --
-------------------------------------------------------------------------------

-- | Transforms an HST boxed mark into a GHC boxity.
transformBoxed :: S.Boxed -> GHC.Boxity
transformBoxed S.Boxed   = GHC.Boxed
transformBoxed S.Unboxed = GHC.Unboxed

-- | Transforms an HST expression into a GHC located expression.
transformExp :: S.Exp GHC -> GHC.LHsExpr GHC.GhcPs
transformExp (S.Var s name) =
  let exp' = case name of
        S.Special _ (S.ExprHole _) -> GHC.HsUnboundVar
          GHC.NoExtField
          (GHC.TrueExprHole (GHC.mkOccName GHC.varName "_"))
          -- TODO Could this be in another name space?
        _ -> GHC.HsVar GHC.NoExtField (transformQName GHC.varName name)
  in  GHC.L (transformSrcSpan s) exp'
transformExp (S.Con s name) = GHC.L
  (transformSrcSpan s)
  (GHC.HsVar GHC.NoExtField (transformQName GHC.dataName name))
transformExp (S.Lit s (Lit lit)) =
  GHC.L (transformSrcSpan s) (GHC.HsLit GHC.NoExtField lit)
transformExp (S.Lit s (OverLit lit)) =
  GHC.L (transformSrcSpan s) (GHC.HsOverLit GHC.NoExtField lit)
transformExp (S.InfixApp s e1 qOp e2) = GHC.L
  (transformSrcSpan s)
  (GHC.OpApp GHC.NoExtField
             (transformExp e1)
             (transformQOp qOp)
             (transformExp e2)
  )
transformExp (S.App s e1 e2) = GHC.L
  (transformSrcSpan s)
  (GHC.HsApp GHC.NoExtField (transformExp e1) (transformExp e2))
transformExp (S.NegApp s e) = GHC.L
  (transformSrcSpan s)
  (GHC.NegApp GHC.NoExtField (transformExp e) GHC.noSyntaxExpr)
transformExp (S.Lambda s pats e) =
  let s'    = transformSrcSpan s
      match = S.Match s
                      (S.Ident S.NoSrcSpan "")
                      pats
                      (S.UnGuardedRhs (S.getSrcSpan e) e)
                      Nothing
  in  GHC.L s'
            (GHC.HsLam GHC.NoExtField (transformMatches LambdaExp s' [match]))
transformExp (S.Let s binds e) = GHC.L
  (transformSrcSpan s)
  (GHC.HsLet GHC.NoExtField (transformMaybeBinds (Just binds)) (transformExp e))
transformExp (S.If s e1 e2 e3) = GHC.L
  (transformSrcSpan s)
  (GHC.HsIf GHC.NoExtField
            (Just GHC.noSyntaxExpr)
            (transformExp e1)
            (transformExp e2)
            (transformExp e3)
  )
-- TODO Is Nothing instead of Just GHC.noSyntaxExpr possible as well?
transformExp (S.Case s e alts) =
  let s' = transformSrcSpan s
  in  GHC.L
        s'
        (GHC.HsCase GHC.NoExtField (transformExp e) (transformAlts s' alts))
transformExp (S.Tuple s boxed es) = GHC.L
  (transformSrcSpan s)
  (GHC.ExplicitTuple GHC.NoExtField
                     (map transformExpTuple es)
                     (transformBoxed boxed)
  )
 where
  transformExpTuple :: S.Exp GHC -> GHC.LHsTupArg GHC.GhcPs
  transformExpTuple e' = GHC.L (transformSrcSpan (S.getSrcSpan e'))
                               (GHC.Present GHC.NoExtField (transformExp e'))
transformExp (S.List s es) = GHC.L
  (transformSrcSpan s)
  (GHC.ExplicitList GHC.NoExtField Nothing (map transformExp es))
-- TODO Is Just GHC.noSyntaxExpr instead of Nothing possible as well?
transformExp (S.Paren s e) =
  GHC.L (transformSrcSpan s) (GHC.HsPar GHC.NoExtField (transformExp e))
transformExp (S.ExpTypeSig s e (SigType typ)) = GHC.L
  (transformSrcSpan s)
  (GHC.ExprWithTySig GHC.NoExtField (transformExp e) typ)

transformAlts
  :: GHC.SrcSpan
  -> [S.Alt GHC]
  -> GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
-- The source span information of the group of case alternatives seems to be
-- missing in the HSE syntax and therefore in our syntax, so the source span
-- of the entire case construct is inserted instead
transformAlts s alts = transformMatches CaseAlt s (map altToMatch alts)
 where
  altToMatch :: S.Alt GHC -> S.Match GHC
  altToMatch (S.Alt s' pat rhs mBinds) =
    S.Match s' (S.Ident S.NoSrcSpan "") [pat] rhs mBinds

-------------------------------------------------------------------------------
-- Patterns                                                                  --
-------------------------------------------------------------------------------

-- | Transforms an HST pattern into a GHC located pattern.
transformPat :: S.Pat GHC -> GHC.LPat GHC.GhcPs
transformPat (S.PVar s name) = GHC.L
  (transformSrcSpan s)
  (GHC.VarPat GHC.NoExtField (transformName GHC.varName name))
transformPat (S.PInfixApp s pat1 qName pat2) = GHC.L
  (transformSrcSpan s)
  (GHC.ConPatIn (transformQName GHC.dataName qName)
                (GHC.InfixCon (transformPat pat1) (transformPat pat2))
  )
transformPat (S.PApp s qName pats) = GHC.L
  (transformSrcSpan s)
  (GHC.ConPatIn (transformQName GHC.dataName qName)
                (GHC.PrefixCon (map transformPat pats))
  )
transformPat (S.PTuple s boxed pats) = GHC.L
  (transformSrcSpan s)
  (GHC.TuplePat GHC.NoExtField (map transformPat pats) (transformBoxed boxed))
transformPat (S.PParen s pat) =
  GHC.L (transformSrcSpan s) (GHC.ParPat GHC.NoExtField (transformPat pat))
transformPat (S.PList s pats) = GHC.L
  (transformSrcSpan s)
  (GHC.ListPat GHC.NoExtField (map transformPat pats))
transformPat (S.PWildCard s) =
  GHC.L (transformSrcSpan s) (GHC.WildPat GHC.NoExtField)

-------------------------------------------------------------------------------
-- Names                                                                     --
-------------------------------------------------------------------------------

-- | Transforms an HST module name into a GHC module name.
transformModuleName :: S.ModuleName GHC -> GHC.ModuleName
transformModuleName (S.ModuleName _ str) = GHC.mkModuleName str

-- | Transforms an HST qualified name with GHC name space into a GHC located
--   reader name.
transformQName :: GHC.NameSpace -> S.QName GHC -> GHC.Located GHC.RdrName
transformQName nameSpace (S.Qual s modName name) = GHC.L
  (transformSrcSpan s)
  (GHC.Qual (transformModuleName modName) (transformNameOcc nameSpace name))
transformQName nameSpace (S.UnQual s name) =
  GHC.L (transformSrcSpan s) (GHC.Unqual (transformNameOcc nameSpace name))
transformQName _ (S.Special s spCon) =
  GHC.L (transformSrcSpan s) (GHC.Exact (transformSpecialCon spCon))

-- | Transforms an HST name with GHC name space into a GHC located reader name.
transformName :: GHC.NameSpace -> S.Name GHC -> GHC.Located GHC.RdrName
transformName nameSpace (S.Ident s str) =
  GHC.L (transformSrcSpan s) (GHC.Unqual (GHC.mkOccName nameSpace str))
transformName nameSpace (S.Symbol s str) =
  GHC.L (transformSrcSpan s) (GHC.Unqual (GHC.mkOccName nameSpace str))

-- | Transforms an HST name with GHC name space into a GHC occurrence name.
transformNameOcc :: GHC.NameSpace -> S.Name GHC -> GHC.OccName
transformNameOcc nameSpace (S.Ident  _ str) = GHC.mkOccName nameSpace str
transformNameOcc nameSpace (S.Symbol _ str) = GHC.mkOccName nameSpace str

-- | Transforms an HST qualified operator into a GHC located expression.
transformQOp :: S.QOp GHC -> GHC.LHsExpr GHC.GhcPs
transformQOp (S.QVarOp s qName) = GHC.L
  (transformSrcSpan s)
  (GHC.HsVar GHC.NoExtField (transformQName GHC.varName qName))
transformQOp (S.QConOp s qName) = GHC.L
  (transformSrcSpan s)
  (GHC.HsVar GHC.NoExtField (transformQName GHC.dataName qName))

-- | Transforms an HST special constructor into a GHC name.
--
--   Expression holes appear at expression level in the GHC AST and are
--   transformed in 'transformExp' instead.
transformSpecialCon :: S.SpecialCon GHC -> GHC.Name
transformSpecialCon (S.UnitCon _) = GHC.tyConName GHC.unitTyCon
transformSpecialCon (S.ListCon _) = GHC.listTyConName
transformSpecialCon (S.FunCon  _) = GHC.tyConName GHC.funTyCon
transformSpecialCon (S.TupleCon _ boxed arity) =
  GHC.tyConName (GHC.tupleTyCon (transformBoxed boxed) arity)
transformSpecialCon (S.Cons _) = GHC.consDataConName
transformSpecialCon (S.UnboxedSingleCon _) =
  GHC.tupleTyConName GHC.UnboxedTuple 0
transformSpecialCon (S.ExprHole _) =
  error "Expression holes should be transformed in transformExp"

-------------------------------------------------------------------------------
-- Source Spans                                                              --
-------------------------------------------------------------------------------

-- | Unwraps the HST type for source spans into an GHC source span.
transformSrcSpan :: S.SrcSpan GHC -> GHC.SrcSpan
transformSrcSpan (S.SrcSpan s) = s
transformSrcSpan S.NoSrcSpan   = GHC.noSrcSpan
