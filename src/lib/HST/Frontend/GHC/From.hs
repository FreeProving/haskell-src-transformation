{-# LANGUAGE PackageImports #-}

-- | This module contains functions transforming Haskell modules and other
--   constructs of the AST data structure of @ghc-lib-parser@ into the
--   corresponding constructs of the AST data structure in the
--   "HST.Frontend.Syntax" module.

module HST.Frontend.GHC.From where


import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( catMaybes
                                                , isJust
                                                )
import qualified "ghc-lib-parser" Bag          as GHC
import qualified "ghc-lib-parser" BasicTypes   as GHC
import qualified "ghc-lib-parser" ConLike      as GHC
import qualified "ghc-lib-parser" DataCon      as GHC
import qualified "ghc-lib-parser" GHC.Hs       as GHC
import qualified "ghc-lib-parser" Module       as GHC
import qualified "ghc-lib-parser" Name         as GHC
import qualified "ghc-lib-parser" RdrName      as GHC
import qualified "ghc-lib-parser" SrcLoc       as GHC
import qualified "ghc-lib-parser" Type         as GHC
import qualified "ghc-lib-parser" TysWiredIn   as GHC
import           Polysemy                       ( Member
                                                , Sem
                                                )

import           HST.Effect.Report              ( Message(Message)
                                                , Report
                                                , Severity(Error)
                                                , notSupported
                                                , reportFatal
                                                , skipNotSupported
                                                )
import           HST.Frontend.GHC.Config        ( GHC
                                                , LitWrapper(Lit, OverLit)
                                                , TypeWrapper(SigType)
                                                , OriginalModuleHead
                                                  ( OriginalModuleHead
                                                  )
                                                , DeclWrapper(Decl)
                                                )
import qualified HST.Frontend.Syntax           as S

-------------------------------------------------------------------------------
-- Modules                                                                   --
-------------------------------------------------------------------------------

-- | Transforms the @ghc-lib-parser@ representation of a located Haskell module
--   into the @haskell-src-transformations@ representation of a Haskell module.
transformModule
  :: Member Report r
  => GHC.Located (GHC.HsModule GHC.GhcPs)
  -> Sem r (S.Module GHC)
transformModule (GHC.L s modul) =
  let modName' = case GHC.hsmodName modul of
        Just (GHC.L s' modName) ->
          Just (transformModuleName (transformSrcSpan s') modName)
        Nothing -> Nothing
  in  S.Module
          (transformSrcSpan s)
          (OriginalModuleHead (GHC.hsmodName modul)
                              (GHC.hsmodExports modul)
                              (GHC.hsmodImports modul)
                              (GHC.hsmodDeprecMessage modul)
                              (GHC.hsmodHaddockModHeader modul)
          )
          modName'
        <$> mapM transformDecl (GHC.hsmodDecls modul)

-------------------------------------------------------------------------------
-- Declarations                                                              --
-------------------------------------------------------------------------------

-- | Transforms a GHC located declaration into an HST declaration.
--
--   Unsupported declarations are preserved by wrapping them in the
--   'S.OtherDecl' constructor.
transformDecl :: Member Report r => GHC.LHsDecl GHC.GhcPs -> Sem r (S.Decl GHC)

-- Data type and newtype declarations are supported.
transformDecl decl@(GHC.L s (GHC.TyClD _ dataDecl@GHC.DataDecl{})) = do
  mDataDefn <- transformDataDefn (GHC.tcdDataDefn dataDecl)
  case mDataDefn of
    Nothing       -> return $ S.OtherDecl (transformSrcSpan s) (Decl decl)
    Just dataDefn -> do
      name <- transformRdrNameUnqual (GHC.tcdLName dataDecl)
      return $ S.DataDecl (transformSrcSpan s) (Decl decl) name dataDefn

-- Function declarations are supported.
transformDecl (GHC.L s (GHC.ValD _ fb@GHC.FunBind{})) =
  S.FunBind (transformSrcSpan s) <$> transformMatchGroup (GHC.fun_matches fb)

-- Type and data families, type declarations, type classes and extension
-- declarations are not supported and therefore skipped. The user is explicitly
-- informed about skipped type classes since they might contain pattern
-- matching.
transformDecl decl@(GHC.L s (GHC.TyClD _ GHC.FamDecl{})) =
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.TyClD _ GHC.SynDecl{})) =
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.TyClD _ GHC.ClassDecl{})) = do
  skipNotSupported "Type classes"
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.TyClD _ (GHC.XTyClDecl _))) =
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)

-- Type class instances, data family instances, type family instances and
-- extension declarations are not supported and therefore skipped. The user is
-- explicitly informed about the first two since they might contain pattern
-- matching.
transformDecl decl@(GHC.L s (GHC.InstD _ GHC.ClsInstD{})) = do
  skipNotSupported "Type class instances"
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.InstD _ GHC.DataFamInstD{})) = do
  skipNotSupported "Data family instances"
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.InstD _ GHC.TyFamInstD{})) =
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.InstD _ (GHC.XInstDecl _))) =
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)

-- More complex pattern bindings (pattern binds are never simple in the GHC
-- AST), abstraction bindings and pattern synonyms are not supported and
-- therefore skipped. The user is explicitly informed about this since there
-- may be errors due to this.
transformDecl decl@(GHC.L s (GHC.ValD _ GHC.PatBind{})) = do
  skipNotSupported "Non-variable pattern bindings"
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.ValD _ GHC.AbsBinds{})) = do
  skipNotSupported "Abstraction bindings"
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.ValD _ (GHC.PatSynBind _ _))) = do
  skipNotSupported "Pattern synonyms"
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)

--  Variable bindings and extensions shouldn't occur in the AST after parsing.
transformDecl decl@(GHC.L s (GHC.ValD _ GHC.VarBind{})) =
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.ValD _ (GHC.XHsBindsLR _))) =
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)

-- Template Haskell is not supported. The user is informed when
-- splices are skipped since they contain expressions that are
-- not transformed.
transformDecl decl@(GHC.L s (GHC.SpliceD _ _)) = do
  skipNotSupported "Template Haskell splicing declarations"
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)

-- All other declarations are skipped silently.
transformDecl decl@(GHC.L s (GHC.DerivD _ _)) =
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.SigD _ _)) =
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.KindSigD _ _)) =
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.DefD _ _)) =
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.ForD _ _)) =
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.WarningD _ _)) =
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.AnnD _ _)) =
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.RuleD _ _)) =
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.DocD _ _)) =
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.RoleAnnotD _ _)) =
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.XHsDecl _)) =
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)

-------------------------------------------------------------------------------
-- Data Type Declarations                                                    --
-------------------------------------------------------------------------------

-- | Transforms a GHC data definition into HST constructor declarations.
--
--   The result is wrapped inside the @Maybe@ type since some kinds of data
--   definitions are not supported by the pattern matching compiler and are
--   therefore skipped. @Nothing@ is returned if the data definition itself or
--   any of its constructors is not supported.
transformDataDefn
  :: Member Report r
  => GHC.HsDataDefn GHC.GhcPs
  -> Sem r (Maybe [S.ConDecl GHC])
transformDataDefn GHC.HsDataDefn { GHC.dd_cons = cons } = do
  conDecls <- mapM transformConDecl cons
  return $ if all isJust conDecls then Just (catMaybes conDecls) else Nothing
transformDataDefn (GHC.XHsDataDefn _) = return Nothing

-- | Transforms a GHC located constructor declaration into an HST constructor
--   declaration.
--
--   The result is wrapped inside the @Maybe@ type since some kinds of
--   constructors are not supported by the pattern matching compiler in which
--   case the corresponding data definition is skipped.
transformConDecl
  :: Member Report r => GHC.LConDecl GHC.GhcPs -> Sem r (Maybe (S.ConDecl GHC))
transformConDecl (GHC.L s conDecl@GHC.ConDeclH98{}) = do
  name <- transformRdrNameUnqual (GHC.con_name conDecl)
  transformConDetails (transformSrcSpan s) name (GHC.con_args conDecl)
transformConDecl (GHC.L _ GHC.ConDeclGADT{}) = do
  skipNotSupported "GADT constructors"
  return Nothing
transformConDecl (GHC.L _ (GHC.XConDecl _)) = return Nothing

-- | Transforms an HST constructor name and GHC constructor details into an HST
--   constructor declaration.
--
--   The result is wrapped inside the @Maybe@ type since some kinds of
--   constructors are not supported by the pattern matching compiler in which
--   case the corresponding data definition is skipped.
transformConDetails
  :: Member Report r
  => S.SrcSpan GHC
  -> S.Name GHC
  -> GHC.HsConDetails (GHC.LBangType GHC.GhcPs) recType
  -> Sem r (Maybe (S.ConDecl GHC))
transformConDetails s name (GHC.PrefixCon args) = return $ Just S.ConDecl
  { S.conDeclSrcSpan = s
  , S.conDeclName    = name
  , S.conDeclArity   = length args
  , S.conDeclIsInfix = False
  }
transformConDetails s name (GHC.InfixCon _ _) = return $ Just S.ConDecl
  { S.conDeclSrcSpan = s
  , S.conDeclName    = name
  , S.conDeclArity   = 2
  , S.conDeclIsInfix = True
  }
-- TODO Maybe use a Symbol instead of an Ident name for InfixCon (does that make a difference?)
transformConDetails _ _ (GHC.RecCon _) = do
  skipNotSupported "Record constructors"
  return Nothing

-------------------------------------------------------------------------------
-- Function Declarations                                                     --
-------------------------------------------------------------------------------

-- | Transforms a GHC located binding group into an HST binding group.
transformLocalBinds
  :: Member Report r
  => GHC.LHsLocalBinds GHC.GhcPs
  -> Sem r (Maybe (S.Binds GHC))
transformLocalBinds (GHC.L s (GHC.HsValBinds _ binds)) = do
  binds' <- transformValBinds binds
  return $ Just (S.BDecls (transformSrcSpan s) binds')
transformLocalBinds (GHC.L _ (GHC.EmptyLocalBinds _)) = return Nothing
transformLocalBinds (GHC.L _ (GHC.HsIPBinds _ _)) =
  notSupported "Implicit-parameters"
transformLocalBinds (GHC.L _ (GHC.XHsLocalBindsLR _)) =
  notSupported "Local bindings extensions"

-- | Transforms GHC value bindings into HST declarations.
transformValBinds
  :: Member Report r => GHC.HsValBinds GHC.GhcPs -> Sem r [S.Decl GHC]
transformValBinds (GHC.ValBinds _ binds sigs) = mapM
  transformDecl
  (  map (\(GHC.L s bind) -> GHC.L s (GHC.ValD GHC.NoExtField bind))
         (GHC.bagToList binds)
  ++ map (\(GHC.L s sig) -> GHC.L s (GHC.SigD GHC.NoExtField sig)) sigs
  )
transformValBinds (GHC.XValBindsLR _) =
  notSupported "Value bindings extensions"

-- | Transforms a GHC match group into HST matches.
transformMatchGroup
  :: Member Report r
  => GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
  -> Sem r [S.Match GHC]
transformMatchGroup GHC.MG { GHC.mg_alts = GHC.L _ matches } =
  mapM transformMatch matches
transformMatchGroup (GHC.XMatchGroup _) = notSupported "Match group extensions"

-- | Transforms a GHC located match into an HST match.
transformMatch
  :: Member Report r
  => GHC.LMatch GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
  -> Sem r (S.Match GHC)
transformMatch (GHC.L s match@GHC.Match{}) = do
  let s' = transformSrcSpan s
  (name', fixity) <- case GHC.m_ctxt match of
    ctxt@GHC.FunRhs{} -> do
      name <- transformRdrNameUnqual (GHC.mc_fun ctxt)
      return (name, GHC.mc_fixity ctxt)
    _ -> return $ (S.Ident S.NoSrcSpan "", GHC.Prefix)
  pats          <- mapM transformPat (GHC.m_pats match)
  (rhs, mBinds) <- transformGRHSs (GHC.m_grhss match)
  return $ case fixity of
    GHC.Prefix -> S.Match s' name' pats rhs mBinds
    GHC.Infix  -> S.InfixMatch s' (head pats) name' (tail pats) rhs mBinds
transformMatch (GHC.L _ (GHC.XMatch _)) = notSupported "Match extensions"

-- | Transforms GHC guarded right-hand sides into an HST right-hand side and
--   binding group.
transformGRHSs
  :: Member Report r
  => GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
  -> Sem r (S.Rhs GHC, Maybe (S.Binds GHC))
transformGRHSs grhss@GHC.GRHSs{} = do
  binds <- transformLocalBinds (GHC.grhssLocalBinds grhss)
  case GHC.grhssGRHSs grhss of
    [GHC.L s (GHC.GRHS _ [] body)] -> do
      body' <- transformExpr body
      return (S.UnGuardedRhs (transformSrcSpan s) body', binds)
    grhss' -> do
      grhss'' <- mapM transformGRHS grhss'
      return (S.GuardedRhss S.NoSrcSpan grhss'', binds)
        -- The source span here seems to be missing in the GHC AST
transformGRHSs (GHC.XGRHSs _) =
  notSupported "Guarded right-hand sides extensions"

-- | Transforms a GHC guarded right-hand side into an HST guarded right-hand
--   side.
transformGRHS
  :: Member Report r
  => GHC.LGRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
  -> Sem r (S.GuardedRhs GHC)
transformGRHS (GHC.L s (GHC.GRHS _ [gStmt] body)) =
  S.GuardedRhs (transformSrcSpan s)
    <$> transformStmtExpr gStmt
    <*> transformExpr body
transformGRHS (GHC.L _ (GHC.GRHS _ _ _)) =
  notSupported "Guarded right-hand sides without exactly one guard"
transformGRHS (GHC.L _ (GHC.XGRHS _)) =
  notSupported "Guarded right-hand side extensions"

-- | Transforms a GHC located statement consisting only of a single expression
--   into an HST expression.
transformStmtExpr
  :: Member Report r
  => GHC.LStmt GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
  -> Sem r (S.Exp GHC)
transformStmtExpr (GHC.L _ (GHC.BodyStmt _ body _ _)) = transformExpr body
transformStmtExpr _ = notSupported "Complex guards"
-- TODO Are there more statements that can be safely converted to boolean expressions?

-------------------------------------------------------------------------------
-- Expressions                                                               --
-------------------------------------------------------------------------------

-- | Transforms a GHC boxity into an HST boxed mark.
transformBoxity :: GHC.Boxity -> S.Boxed
transformBoxity GHC.Boxed   = S.Boxed
transformBoxity GHC.Unboxed = S.Unboxed

-- | Transforms a GHC located expression into an HST expression.
transformExpr :: Member Report r => GHC.LHsExpr GHC.GhcPs -> Sem r (S.Exp GHC)
transformExpr (GHC.L s (GHC.HsVar _ name)) = do
  let s' = transformSrcSpan s
  name' <- transformRdrName name
  return $ case name' of
    (qName, False) -> S.Var s' qName
    (qName, True ) -> S.Con s' qName
transformExpr (GHC.L s (GHC.HsUnboundVar _ _)) =
  let s' = transformSrcSpan s
  in  return $ S.Var s' (S.Special s' (S.ExprHole s'))
transformExpr (GHC.L s (GHC.HsLit _ lit)) =
  return $ S.Lit (transformSrcSpan s) (Lit lit)
transformExpr (GHC.L s (GHC.HsOverLit _ lit)) =
  return $ S.Lit (transformSrcSpan s) (OverLit lit)
transformExpr (GHC.L s (GHC.OpApp _ e1 op e2)) = do
  e1'  <- transformExpr e1
  e2'  <- transformExpr e2
  op'  <- transformExpr op
  op'' <- case op' of
    (S.Var s' name) -> return $ S.QVarOp s' name
    (S.Con s' name) -> return $ S.QConOp s' name
    _ -> notSupported "Infix operators that aren't variables or constructors"
  return $ S.InfixApp (transformSrcSpan s) e1' op'' e2'
transformExpr (GHC.L s (GHC.HsApp _ e1 e2)) =
  S.App (transformSrcSpan s) <$> transformExpr e1 <*> transformExpr e2
transformExpr (GHC.L s (GHC.NegApp _ e _)) =
  S.NegApp (transformSrcSpan s) <$> transformExpr e
transformExpr (GHC.L s (GHC.HsLam _ mg)) = do
  mg' <- transformMatchGroup mg
  case mg' of
    [S.Match _ _ pats (S.UnGuardedRhs _ e) Nothing] ->
      return $ S.Lambda (transformSrcSpan s) pats e
    [S.Match _ _ _ _ (Just _)] ->
      notSupported "Lambda abstractions with bindings"
    [S.Match _ _ _ (S.GuardedRhss _ _) _] ->
      notSupported "Lambda abstractions with guards"
    [S.InfixMatch _ _ _ _ _ _] -> notSupported "Infix lambda abstractions"
    [] -> notSupported "Empty lambda abstractions"
    (_ : _ : _) -> notSupported "Lambda abstractions with multiple matches"
transformExpr (GHC.L s (GHC.HsLet _ binds e)) = do
  mBinds <- transformLocalBinds binds
  case mBinds of
    Nothing     -> notSupported "Let expressions with empty bindings"
    Just binds' -> S.Let (transformSrcSpan s) binds' <$> transformExpr e
transformExpr (GHC.L s (GHC.HsIf _ _ e1 e2 e3)) =
  S.If (transformSrcSpan s)
    <$> transformExpr e1
    <*> transformExpr e2
    <*> transformExpr e3
transformExpr (GHC.L s (GHC.HsCase _ e mg)) = do
  e'   <- transformExpr e
  mg'  <- transformMatchGroup mg
  alts <- mapM matchToAlt mg'
  return $ S.Case (transformSrcSpan s) e' alts
 where
  matchToAlt :: Member Report r => S.Match GHC -> Sem r (S.Alt GHC)
  matchToAlt (S.Match s' _ [pat] rhs mBinds) = return $ S.Alt s' pat rhs mBinds
  matchToAlt (S.Match _ _ _ _ _) =
    notSupported "Case alternatives without exactly one pattern"
  matchToAlt (S.InfixMatch _ _ _ _ _ _) =
    notSupported "Infix matches in case alternatives"
transformExpr (GHC.L s (GHC.ExplicitTuple _ tArgs boxity)) =
  S.Tuple (transformSrcSpan s) (transformBoxity boxity)
    <$> mapM transformTupleArg tArgs
transformExpr (GHC.L s (GHC.ExplicitList _ _ es)) =
  S.List (transformSrcSpan s) <$> mapM transformExpr es
transformExpr (GHC.L s (GHC.HsPar _ e)) =
  S.Paren (transformSrcSpan s) <$> transformExpr e
transformExpr (GHC.L s (GHC.ExprWithTySig _ e typeSig)) = do
  e' <- transformExpr e
  return $ S.ExpTypeSig (transformSrcSpan s) e' (SigType typeSig)

-- All other expressions are not supported.
transformExpr (GHC.L _ (GHC.HsConLikeOut _ _)) =
  notSupported "Expressions introduced by the type checker"
transformExpr (GHC.L _ (GHC.HsRecFld _ _)) = notSupported "Records"
transformExpr (GHC.L _ (GHC.HsOverLabel _ _ _)) =
  notSupported "Overloaded labels"
transformExpr (GHC.L _ (GHC.HsIPVar _ _)) = notSupported "Implicit parameters"
transformExpr (GHC.L _ (GHC.HsLamCase _ _)) =
  notSupported "Lambda-case-expressions"
transformExpr (GHC.L _ (GHC.HsAppType _ _ _)) =
  notSupported "Visible type applications"
transformExpr (GHC.L _ (GHC.SectionL _ _ _     )) = notSupported "Sections"
transformExpr (GHC.L _ (GHC.SectionR _ _ _     )) = notSupported "Sections"
transformExpr (GHC.L _ (GHC.ExplicitSum _ _ _ _)) = notSupported "Unboxed sums"
transformExpr (GHC.L _ (GHC.HsMultiIf _ _)) =
  notSupported "Multi-way if-expressions"
transformExpr (GHC.L _ (GHC.HsDo _ _ _)) = notSupported "do-expressions"
transformExpr (GHC.L _ GHC.RecordCon{} ) = notSupported "Records"
transformExpr (GHC.L _ GHC.RecordUpd{} ) = notSupported "Records"
transformExpr (GHC.L _ (GHC.ArithSeq _ _ _)) =
  notSupported "Arithmetic sequences"
transformExpr (GHC.L _ (GHC.HsSCC _ _ _ _)) =
  notSupported "Set-cost-centre-expressions"
transformExpr (GHC.L _ (GHC.HsCoreAnn _ _ _ _)) =
  notSupported "Core annotations"
transformExpr (GHC.L _ (GHC.HsBracket _ _)) =
  notSupported "Template Haskell expressions"
transformExpr (GHC.L _ (GHC.HsRnBracketOut _ _ _)) =
  notSupported "Template Haskell expressions"
transformExpr (GHC.L _ (GHC.HsTcBracketOut _ _ _)) =
  notSupported "Template Haskell expressions"
transformExpr (GHC.L _ (GHC.HsSpliceE _ _)) =
  notSupported "Template Haskell expressions"
transformExpr (GHC.L _ (GHC.HsProc _ _ _)) = notSupported "Arrow expressions"
transformExpr (GHC.L _ (GHC.HsStatic _ _)) = notSupported "Static pointers"
transformExpr (GHC.L _ (GHC.HsTick _ _ _)) =
  notSupported "Haskell program coverage"
transformExpr (GHC.L _ (GHC.HsBinTick _ _ _ _)) =
  notSupported "Haskell program coverage"
transformExpr (GHC.L _ (GHC.HsTickPragma _ _ _ _ _)) =
  notSupported "Haskell program coverage"
transformExpr (GHC.L _ (GHC.HsWrap _ _ _)) =
  notSupported "Expressions introduced by the type checker"
transformExpr (GHC.L _ (GHC.XExpr _)) = notSupported "Expression extensions"


-- | Transforms a GHC located tuple argument consisting of an expression into
--   an HST expression.
transformTupleArg
  :: Member Report r => GHC.LHsTupArg GHC.GhcPs -> Sem r (S.Exp GHC)
transformTupleArg (GHC.L _ (GHC.Present _ e)) = transformExpr e
transformTupleArg (GHC.L _ (GHC.Missing _)) =
  notSupported "Missing tuple arguments"
transformTupleArg (GHC.L _ (GHC.XTupArg _)) =
  notSupported "Tuple argument extensions"

-------------------------------------------------------------------------------
-- Patterns                                                                  --
-------------------------------------------------------------------------------

-- | Transforms a GHC located pattern into an HST pattern.
transformPat :: Member Report r => GHC.LPat GHC.GhcPs -> Sem r (S.Pat GHC)
transformPat (GHC.L s (GHC.VarPat _ name)) =
  S.PVar (transformSrcSpan s) <$> transformRdrNameUnqual name
transformPat (GHC.L s (GHC.ConPatIn name cpds)) = do
  let s' = transformSrcSpan s
  (name', isCon) <- transformRdrName name
  case (cpds, isCon) of
    (GHC.InfixCon pat1 pat2, True) ->
      S.PInfixApp s'
        <$> transformPat pat1
        <*> return name'
        <*> transformPat pat2
    (GHC.PrefixCon pats, True) -> S.PApp s' name' <$> mapM transformPat pats
    (_, True) -> notSupported "Record constructors are not supported"
    _ -> notSupported "Only constructors can be applied in patterns"
-- TODO The documentation also mentions a more complicated ConPatOut.
-- Do we need to consider that?
transformPat (GHC.L s (GHC.TuplePat _ pats boxity)) =
  S.PTuple (transformSrcSpan s) (transformBoxity boxity)
    <$> mapM transformPat pats
transformPat (GHC.L s (GHC.ParPat _ pat)) =
  S.PParen (transformSrcSpan s) <$> transformPat pat
transformPat (GHC.L s (GHC.ListPat _ pats)) =
  S.PList (transformSrcSpan s) <$> mapM transformPat pats
transformPat (GHC.L s (GHC.WildPat _)) =
  return $ S.PWildCard (transformSrcSpan s)

-- All other patterns are not supported.
transformPat (GHC.L _ (GHC.LazyPat _ _)) = notSupported "Lazy patterns"
transformPat (GHC.L _ (GHC.AsPat _ _ _)) = notSupported "as-patterns"
transformPat (GHC.L _ (GHC.BangPat _ _)) = notSupported "Bang patterns"
transformPat (GHC.L _ (GHC.SumPat _ _ _ _)) =
  notSupported "Anonymous sum patterns"
transformPat (GHC.L _ GHC.ConPatOut{}) =
  notSupported "Constructor patterns out"
transformPat (GHC.L _ (GHC.ViewPat _ _ _)) = notSupported "View patterns"
transformPat (GHC.L _ (GHC.SplicePat _ _)) = notSupported "Template Haskell"
transformPat (GHC.L _ (GHC.LitPat    _ _)) = notSupported "Literal patterns"
transformPat (GHC.L _ (GHC.NPat _ _ _ _ )) = notSupported "Natural patterns"
transformPat (GHC.L _ (GHC.NPlusKPat _ _ _ _ _ _)) =
  notSupported "n+k patterns"
transformPat (GHC.L _ (GHC.SigPat _ _ _)) =
  notSupported "Patterns with type signature"
transformPat (GHC.L _ (GHC.CoPat _ _ _ _)) = notSupported "Coercion patterns"
transformPat (GHC.L _ (GHC.XPat _       )) = notSupported "Extension patterns"

-------------------------------------------------------------------------------
-- Names                                                                     --
-------------------------------------------------------------------------------

-- | Transforms a GHC module name with an HST source span into an HST module
--   name.
transformModuleName :: S.SrcSpan GHC -> GHC.ModuleName -> S.ModuleName GHC
transformModuleName s modName = S.ModuleName s (GHC.moduleNameString modName)

-- | Transforms a GHC located reader name into an HST qualified name and a
--   @Bool@ which is @True@ if the name belongs to a data constructor and
--   @False@ otherwise.
transformRdrName
  :: Member Report r => GHC.Located GHC.RdrName -> Sem r (S.QName GHC, Bool)
transformRdrName (GHC.L s (GHC.Unqual name)) =
  let s' = transformSrcSpan s
  in  return
        (S.UnQual s' (S.Ident s' (GHC.occNameString name)), GHC.isDataOcc name)
transformRdrName (GHC.L s (GHC.Qual modName name)) =
  let s' = transformSrcSpan s
  in  return
        ( S.Qual s'
                 (transformModuleName s' modName)
                 (S.Ident s' (GHC.occNameString name))
        , GHC.isDataOcc name
        )
transformRdrName (GHC.L s (GHC.Exact name)) = do
  let s' = transformSrcSpan s
  specialCon <- transformSpecialCon s' name
  return (S.Special s' specialCon, True)
transformRdrName (GHC.L _ (GHC.Orig _ _)) = notSupported "Original names"

-- | Transforms a GHC located unqualified reader name into an HST name.
transformRdrNameUnqual
  :: Member Report r => GHC.Located GHC.RdrName -> Sem r (S.Name GHC)
transformRdrNameUnqual (GHC.L s (GHC.Unqual occName)) =
  return $ S.Ident (transformSrcSpan s) (GHC.occNameString occName)
transformRdrNameUnqual (GHC.L _ (GHC.Qual _ _)) =
  notSupported "Qualified names where unqualified names are expected"
transformRdrNameUnqual (GHC.L _ (GHC.Orig _ _)) = notSupported "Original names"
transformRdrNameUnqual (GHC.L _ (GHC.Exact _)) =
  notSupported "Exact names where unqualified names are expected"

-- | Transforms a GHC name with an HST source span into an HST special
--   constructor.
transformSpecialCon
  :: Member Report r => S.SrcSpan GHC -> GHC.Name -> Sem r (S.SpecialCon GHC)
transformSpecialCon s name = case Map.lookup name specialDataConMap of
  Just mkSpecialCon -> return $ mkSpecialCon s
  Nothing           -> case GHC.wiredInNameTyThing_maybe name of
    Just (GHC.AConLike (GHC.RealDataCon dataCon))
      | GHC.isUnboxedTupleCon dataCon
      -> return $ S.TupleCon s S.Unboxed $ GHC.dataConSourceArity dataCon
      | GHC.isTupleDataCon dataCon
      -> return $ S.TupleCon s S.Boxed $ GHC.dataConSourceArity dataCon
    _ ->
      reportFatal
        $ Message Error
        $ (  "Wired in data constructor not supported: "
          ++ GHC.occNameString (GHC.nameOccName name)
          )

-- | Maps GHC names of data constructors to functions that build HST special
--   constructor nodes with the given source span.
--
--   Tuple constructors cannot be transformed with this map and are instead
--   transformed directly in 'transformSpecialCon'.
--   Expression holes appear at expression level in the GHC AST and are
--   transformed in 'transformExpr' instead.
specialDataConMap :: Map GHC.Name (S.SrcSpan GHC -> S.SpecialCon GHC)
specialDataConMap = Map.fromList
  [ (GHC.dataConName GHC.unitDataCon       , S.UnitCon)
  , (GHC.dataConName GHC.nilDataCon        , S.NilCon)
  , (GHC.dataConName GHC.consDataCon       , S.ConsCon)
  , (GHC.dataConName GHC.unboxedUnitDataCon, S.UnboxedSingleCon)
  ]

-------------------------------------------------------------------------------
-- Source Spans                                                              --
-------------------------------------------------------------------------------

-- | Wraps a GHC source span into the HST type for source spans.
transformSrcSpan :: GHC.SrcSpan -> S.SrcSpan GHC
transformSrcSpan = S.SrcSpan
