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

import qualified Bag                     as GHC
import qualified BasicTypes              as GHC
import qualified DataCon                 as GHC
import qualified GHC.Hs                  as GHC
import qualified Module                  as GHC
import qualified Name                    as GHC
import           Polysemy                ( Member, Sem )
import qualified RdrName                 as GHC
import qualified SrcLoc                  as GHC
import qualified TcEvidence              as GHC
import qualified TysWiredIn              as GHC

import           HST.Effect.Report       ( Report, reportFatal )
import           HST.Frontend.GHC.Config
  ( DeclWrapper(Decl), GHC, LitWrapper(Lit, OverLit)
  , OriginalModuleHead(originalModuleName, originalModuleExports,
                   originalModuleImports, originalModuleDeprecMessage,
                   originalModuleHaddockModHeader)
  , TypeWrapper(SigType) )
import qualified HST.Frontend.GHC.From   as FromGHC
import qualified HST.Frontend.Syntax     as S
import           HST.Util.Messages       ( Severity(Internal), message )

-------------------------------------------------------------------------------
-- Modules                                                                   --
-------------------------------------------------------------------------------
-- | Transforms the @haskell-src-transformations@ representation of a Haskell
--   module into the @ghc-lib-parser@ representation of a located Haskell
--   module.
--
--   The module head is restored from the original module head. The module
--   name field does not affect the name of the resulting module.
transformModule :: Member Report r
                => S.Module GHC
                -> Sem r (GHC.Located (GHC.HsModule GHC.GhcPs))
transformModule (S.Module s omh _ decls) = do
  decls' <- mapM transformDecl decls
  return
    $ GHC.L (transformSrcSpan s) GHC.HsModule
    { GHC.hsmodName             = originalModuleName omh
    , GHC.hsmodExports          = originalModuleExports omh
    , GHC.hsmodImports          = originalModuleImports omh
    , GHC.hsmodDecls            = decls'
    , GHC.hsmodDeprecMessage    = originalModuleDeprecMessage omh
    , GHC.hsmodHaddockModHeader = originalModuleHaddockModHeader omh
    }

-------------------------------------------------------------------------------
-- Declarations                                                              --
-------------------------------------------------------------------------------
-- | Transforms an HST declaration into an GHC located declaration.
transformDecl :: Member Report r => S.Decl GHC -> Sem r (GHC.LHsDecl GHC.GhcPs)
transformDecl (S.DataDecl _ (Decl oDecl) _ _) = return oDecl
transformDecl (S.FunBind s matches) = do
  matchesName <- getMatchesName matches
  let s'    = transformSrcSpan s
      funId = transformName GHC.varName matchesName
  matches' <- transformMatches Function s' matches
  return
    $ GHC.L s' (GHC.ValD GHC.NoExtField GHC.FunBind
                { GHC.fun_ext     = GHC.NoExtField
                , GHC.fun_id      = funId
                , GHC.fun_matches = matches'
                , GHC.fun_co_fn   = GHC.WpHole
                , GHC.fun_tick    = []
                })
 where
  getMatchesName :: Member Report r => [S.Match GHC] -> Sem r (S.Name GHC)
  getMatchesName (S.Match _ name _ _ _ : _) = return name
  getMatchesName (S.InfixMatch _ _ name _ _ _ : _) = return name
  getMatchesName [] = reportFatal
    $ message Internal s
    "Encountered empty match group in function binding during retransformation!"
transformDecl (S.OtherDecl _ (Decl oDecl)) = return oDecl

-------------------------------------------------------------------------------
-- Function Declarations                                                     --
-------------------------------------------------------------------------------
-- | Transforms an HST binding group into a GHC located binding group.
transformMaybeBinds :: Member Report r
                    => Maybe (S.Binds GHC)
                    -> Sem r (GHC.LHsLocalBinds GHC.GhcPs)
transformMaybeBinds Nothing = return
  $ GHC.L GHC.noSrcSpan (GHC.EmptyLocalBinds GHC.NoExtField)
transformMaybeBinds (Just (S.BDecls s decls)) = do
  (funBinds, sigs) <- mapM transformDecl decls >>= splitBDecls
  return
    $ GHC.L (transformSrcSpan s)
    (GHC.HsValBinds GHC.NoExtField
     (GHC.ValBinds GHC.NoExtField (GHC.listToBag funBinds) sigs))
 where
  splitBDecls
    :: Member Report r
    => [GHC.LHsDecl GHC.GhcPs]
    -> Sem r ([GHC.LHsBindLR GHC.GhcPs GHC.GhcPs], [GHC.LSig GHC.GhcPs])
  splitBDecls []              = return $ ([], [])
  splitBDecls (decl : decls') = do
    (funBinds', sigs') <- splitBDecls decls'
    case decl of
      GHC.L s' (GHC.ValD _ fb@GHC.FunBind {}) ->
        return (GHC.L s' fb : funBinds', sigs')
      GHC.L s' (GHC.SigD _ sig) -> return (funBinds', GHC.L s' sig : sigs')
      GHC.L s' _ -> reportFatal
        $ message Internal (FromGHC.transformSrcSpan s')
        $ "Encountered unexpected declaration in binding group during "
        ++ "retransformation. Only function and signature declarations are "
        ++ "allowed!"

-- | Type for the contexts where a match or match group can occur in the GHC
--   AST data structure.
data MatchContext = Function | LambdaExp | CaseAlt
 deriving ( Eq, Show )

-- | Transforms a match context, a GHC source span of the matches and a list of
--   HST matches with into a GHC match group.
transformMatches :: Member Report r
                 => MatchContext
                 -> GHC.SrcSpan
                 -> [S.Match GHC]
                 -> Sem r (GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs))
transformMatches ctxt s matches = do
  matches' <- mapM (transformMatch ctxt) matches
  return GHC.MG { GHC.mg_ext    = GHC.NoExtField
                , GHC.mg_alts   = GHC.L s matches'
                , GHC.mg_origin = GHC.FromSource
                }

-- | Transforms an HST match with a match context into a GHC located match.
transformMatch :: Member Report r
               => MatchContext
               -> S.Match GHC
               -> Sem r (GHC.LMatch GHC.GhcPs (GHC.LHsExpr GHC.GhcPs))
transformMatch ctxt match = do
  let (s, name, pats, rhs, mBinds, fixity) = case match of
        S.Match s' name' pats' rhs' mBinds'          ->
          (s', name', pats', rhs', mBinds', GHC.Prefix)
        S.InfixMatch s' pat name' pats' rhs' mBinds' ->
          (s', name', pat : pats', rhs', mBinds', GHC.Infix)
      ctxt' = case ctxt of
        Function  -> GHC.FunRhs
          { GHC.mc_fun        = transformName GHC.varName name
          , GHC.mc_fixity     = fixity
          , GHC.mc_strictness = GHC.NoSrcStrict
          }
        LambdaExp -> GHC.LambdaExpr
        CaseAlt   -> GHC.CaseAlt
  pats' <- mapM transformPat pats
  grhss <- transformRhs rhs mBinds
  return
    $ GHC.L (transformSrcSpan s) GHC.Match
    { GHC.m_ext   = GHC.NoExtField
    , GHC.m_ctxt  = ctxt'
    , GHC.m_pats  = pats'
    , GHC.m_grhss = grhss
    }

-- | Transforms an HST right-hand side and binding group into GHC guarded
--   right-hand sides.
transformRhs :: Member Report r
             => S.Rhs GHC
             -> Maybe (S.Binds GHC)
             -> Sem r (GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs))
transformRhs rhs mBinds = do
  grhss <- case rhs of
    S.UnGuardedRhs s e     -> do
      e' <- transformExp e
      return [GHC.L (transformSrcSpan s) (GHC.GRHS GHC.NoExtField [] e')]
    S.GuardedRhss _ grhss' -> mapM transformGuardedRhs grhss'
  lBinds <- transformMaybeBinds mBinds
  return GHC.GRHSs { GHC.grhssExt        = GHC.NoExtField
                   , GHC.grhssGRHSs      = grhss
                   , GHC.grhssLocalBinds = lBinds
                   }

-- | Transforms an HST guarded right-hand side into a GHC located guarded
--   right-hand side.
transformGuardedRhs :: Member Report r
                    => S.GuardedRhs GHC
                    -> Sem r (GHC.LGRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs))
transformGuardedRhs (S.GuardedRhs s ge be) = do
  ge' <- transformExp ge
  be' <- transformExp be
  return
    $ GHC.L (transformSrcSpan s)
    (GHC.GRHS GHC.NoExtField
     [ GHC.L (transformSrcSpan (S.getSrcSpan ge))
         (GHC.BodyStmt GHC.NoExtField ge' GHC.noSyntaxExpr GHC.noSyntaxExpr)
     ] be')

-------------------------------------------------------------------------------
-- Expressions                                                               --
-------------------------------------------------------------------------------
-- | Transforms an HST boxed mark into a GHC boxity.
transformBoxed :: S.Boxed -> GHC.Boxity
transformBoxed S.Boxed   = GHC.Boxed
transformBoxed S.Unboxed = GHC.Unboxed

-- | Transforms an HST expression into a GHC located expression.
transformExp :: Member Report r => S.Exp GHC -> Sem r (GHC.LHsExpr GHC.GhcPs)
transformExp (S.Var s name) = do
  exp' <- case name of
    S.Special _ (S.ExprHole _) -> return
      $ GHC.HsUnboundVar GHC.NoExtField
      (GHC.TrueExprHole (GHC.mkOccName GHC.varName "_"))
      -- TODO Could this be in another name space?
    _ -> do
      name' <- transformQName GHC.varName name
      return $ GHC.HsVar GHC.NoExtField name'
  return $ GHC.L (transformSrcSpan s) exp'
transformExp (S.Con s name) = GHC.L (transformSrcSpan s)
  . GHC.HsVar GHC.NoExtField
  <$> transformQName GHC.dataName name
transformExp (S.Lit s (Lit lit)) = return
  $ GHC.L (transformSrcSpan s) (GHC.HsLit GHC.NoExtField lit)
transformExp (S.Lit s (OverLit lit)) = return
  $ GHC.L (transformSrcSpan s) (GHC.HsOverLit GHC.NoExtField lit)
transformExp (S.InfixApp s e1 qOp e2) = GHC.L (transformSrcSpan s)
  <$> (GHC.OpApp GHC.NoExtField <$> transformExp e1
       <*> transformQOp qOp
       <*> transformExp e2)
transformExp (S.App s e1 e2) = GHC.L (transformSrcSpan s)
  <$> (GHC.HsApp GHC.NoExtField <$> transformExp e1 <*> transformExp e2)
transformExp (S.NegApp s e) = GHC.L (transformSrcSpan s)
  <$> (GHC.NegApp GHC.NoExtField <$> transformExp e <*> return GHC.noSyntaxExpr)
transformExp (S.Lambda s pats e)
  = let s'    = transformSrcSpan s
        match = S.Match s (S.Ident S.NoSrcSpan "") pats
          (S.UnGuardedRhs (S.getSrcSpan e) e) Nothing
    in GHC.L s'
       <$> (GHC.HsLam GHC.NoExtField <$> transformMatches LambdaExp s' [match])
transformExp (S.Let s binds e) = GHC.L (transformSrcSpan s)
  <$> (GHC.HsLet GHC.NoExtField <$> transformMaybeBinds (Just binds)
       <*> transformExp e)
transformExp (S.If s e1 e2 e3) = GHC.L (transformSrcSpan s)
  <$> (GHC.HsIf GHC.NoExtField (Just GHC.noSyntaxExpr) <$> transformExp e1
       <*> transformExp e2
       <*> transformExp e3)
-- TODO Is Nothing instead of Just GHC.noSyntaxExpr possible as well?
transformExp (S.Case s e alts)
  = let s' = transformSrcSpan s
    in GHC.L s'
       <$> (GHC.HsCase GHC.NoExtField <$> transformExp e
            <*> transformAlts s' alts)
transformExp (S.Tuple s boxed es) = GHC.L (transformSrcSpan s)
  <$> (GHC.ExplicitTuple GHC.NoExtField <$> mapM transformExpTuple es
       <*> return (transformBoxed boxed))
 where
  transformExpTuple
    :: Member Report r => S.Exp GHC -> Sem r (GHC.LHsTupArg GHC.GhcPs)
  transformExpTuple e' = GHC.L (transformSrcSpan (S.getSrcSpan e'))
    <$> (GHC.Present GHC.NoExtField <$> transformExp e')
transformExp (S.List s es) = GHC.L (transformSrcSpan s)
  <$> (GHC.ExplicitList GHC.NoExtField Nothing <$> mapM transformExp es)
-- TODO Is Just GHC.noSyntaxExpr instead of Nothing possible as well?
transformExp (S.Paren s e) = GHC.L (transformSrcSpan s)
  <$> (GHC.HsPar GHC.NoExtField <$> transformExp e)
transformExp (S.ExpTypeSig s e (SigType typ)) = GHC.L (transformSrcSpan s)
  <$> (GHC.ExprWithTySig GHC.NoExtField <$> transformExp e <*> return typ)

transformAlts :: Member Report r
              => GHC.SrcSpan
              -> [S.Alt GHC]
              -> Sem r (GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs))

-- The source span information of the group of case alternatives seems to be
-- missing in the HSE syntax and therefore in our syntax, so the source span
-- of the entire case construct is inserted instead
transformAlts s alts = transformMatches CaseAlt s (map altToMatch alts)
 where
  altToMatch :: S.Alt GHC -> S.Match GHC
  altToMatch (S.Alt s' pat rhs mBinds) = S.Match s' (S.Ident S.NoSrcSpan "")
    [pat] rhs mBinds

-------------------------------------------------------------------------------
-- Patterns                                                                  --
-------------------------------------------------------------------------------
-- | Transforms an HST pattern into a GHC located pattern.
transformPat :: Member Report r => S.Pat GHC -> Sem r (GHC.LPat GHC.GhcPs)
transformPat (S.PVar s name)                 = return
  $ GHC.L (transformSrcSpan s)
  (GHC.VarPat GHC.NoExtField (transformName GHC.varName name))
transformPat (S.PInfixApp s pat1 qName pat2) = GHC.L (transformSrcSpan s)
  <$> (GHC.ConPatIn <$> transformQName GHC.dataName qName
       <*> (GHC.InfixCon <$> transformPat pat1 <*> transformPat pat2))
transformPat (S.PApp s qName pats)           = GHC.L (transformSrcSpan s)
  <$> (GHC.ConPatIn <$> transformQName GHC.dataName qName
       <*> (GHC.PrefixCon <$> mapM transformPat pats))
transformPat (S.PTuple s boxed pats)         = GHC.L (transformSrcSpan s)
  <$> (GHC.TuplePat GHC.NoExtField <$> mapM transformPat pats
       <*> return (transformBoxed boxed))
transformPat (S.PParen s pat)                = GHC.L (transformSrcSpan s)
  <$> (GHC.ParPat GHC.NoExtField <$> transformPat pat)
transformPat (S.PList s pats)                = GHC.L (transformSrcSpan s)
  <$> (GHC.ListPat GHC.NoExtField <$> mapM transformPat pats)
transformPat (S.PWildCard s)                 = return
  $ GHC.L (transformSrcSpan s) (GHC.WildPat GHC.NoExtField)

-------------------------------------------------------------------------------
-- Names                                                                     --
-------------------------------------------------------------------------------
-- | Transforms an HST module name into a GHC module name.
transformModuleName :: S.ModuleName GHC -> GHC.ModuleName
transformModuleName (S.ModuleName _ str) = GHC.mkModuleName str

-- | Transforms an HST qualified name with GHC name space into a GHC located
--   reader name.
transformQName :: Member Report r
               => GHC.NameSpace
               -> S.QName GHC
               -> Sem r (GHC.Located GHC.RdrName)
transformQName nameSpace (S.Qual s modName name) = return
  $ GHC.L (transformSrcSpan s)
  (GHC.Qual (transformModuleName modName) (transformNameOcc nameSpace name))
transformQName nameSpace (S.UnQual s name)       = return
  $ GHC.L (transformSrcSpan s) (GHC.Unqual (transformNameOcc nameSpace name))
transformQName _ (S.Special s spCon)             = GHC.L (transformSrcSpan s)
  <$> (GHC.Exact <$> transformSpecialCon spCon)

-- | Transforms an HST name with GHC name space into a GHC located reader name.
transformName :: GHC.NameSpace -> S.Name GHC -> GHC.Located GHC.RdrName
transformName nameSpace (S.Ident s str)  = GHC.L (transformSrcSpan s)
  (GHC.Unqual (GHC.mkOccName nameSpace str))
transformName nameSpace (S.Symbol s str) = GHC.L (transformSrcSpan s)
  (GHC.Unqual (GHC.mkOccName nameSpace str))

-- | Transforms an HST name with GHC name space into a GHC occurrence name.
transformNameOcc :: GHC.NameSpace -> S.Name GHC -> GHC.OccName
transformNameOcc nameSpace (S.Ident _ str)  = GHC.mkOccName nameSpace str
transformNameOcc nameSpace (S.Symbol _ str) = GHC.mkOccName nameSpace str

-- | Transforms an HST qualified operator into a GHC located expression.
transformQOp :: Member Report r => S.QOp GHC -> Sem r (GHC.LHsExpr GHC.GhcPs)
transformQOp (S.QVarOp s qName) = GHC.L (transformSrcSpan s)
  <$> (GHC.HsVar GHC.NoExtField <$> transformQName GHC.varName qName)
transformQOp (S.QConOp s qName) = GHC.L (transformSrcSpan s)
  <$> (GHC.HsVar GHC.NoExtField <$> transformQName GHC.dataName qName)

-- | Transforms an HST special constructor into a GHC name.
--
--   Expression holes appear at expression level in the GHC AST and are
--   transformed in 'transformExp' instead.
transformSpecialCon :: Member Report r => S.SpecialCon GHC -> Sem r GHC.Name
transformSpecialCon (S.UnitCon _)
  = return $ GHC.dataConName GHC.unitDataCon
transformSpecialCon (S.UnboxedSingleCon _)
  = return $ GHC.dataConName GHC.unboxedUnitDataCon
transformSpecialCon (S.TupleCon _ boxed arity) = return
  $ GHC.dataConName (GHC.tupleDataCon (transformBoxed boxed) arity)
transformSpecialCon (S.NilCon _)
  = return $ GHC.dataConName GHC.nilDataCon
transformSpecialCon (S.ConsCon _)
  = return $ GHC.dataConName GHC.consDataCon
transformSpecialCon (S.ExprHole s)             = reportFatal
  $ message Internal s
  $ "Encountered expression hole at name level in retransformation. "
  ++ "Expression holes should be transformed at expression level with "
  ++ "the ghc-lib front end!"

-------------------------------------------------------------------------------
-- Source Spans                                                              --
-------------------------------------------------------------------------------
-- | Unwraps the HST type for source spans into an GHC source span.
transformSrcSpan :: S.SrcSpan GHC -> GHC.SrcSpan
transformSrcSpan (S.SrcSpan originalSrcSpan _) = originalSrcSpan
transformSrcSpan S.NoSrcSpan                   = GHC.noSrcSpan
