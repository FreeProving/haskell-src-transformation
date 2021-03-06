-- | This module contains functions transforming Haskell modules and other
--   constructs of the AST data structure of @ghc-lib-parser@ into the
--   corresponding constructs of the AST data structure in the
--   "HST.Frontend.Syntax" module.
module HST.Frontend.GHC.From where

import qualified Bag                               as GHC
import qualified BasicTypes                        as GHC
import qualified ConLike                           as GHC
import           Data.Map                          ( Map )
import qualified Data.Map                          as Map
import           Data.Maybe                        ( catMaybes, isJust )
import qualified DataCon                           as GHC
import qualified FastString                        as GHC
import qualified GHC.Hs                            as GHC
import qualified Module                            as GHC
import qualified Name                              as GHC
import           Polysemy                          ( Member, Sem )
import qualified RdrName                           as GHC
import qualified SrcLoc                            as GHC
import qualified Type                              as GHC
import qualified TysWiredIn                        as GHC

import           HST.Effect.Report                 ( Report, reportFatal )
import           HST.Frontend.GHC.Config
  ( DeclWrapper(Decl), GHC, LitWrapper(Lit, OverLit)
  , OriginalModuleHead(OriginalModuleHead), TypeWrapper(SigType) )
import           HST.Frontend.GHC.Util.AnyMatch    ( AnyMatch(..) )
import qualified HST.Frontend.Syntax               as S
import           HST.Frontend.Transformer.Messages
  ( notSupported, skipNotSupported )
import           HST.Util.Messages                 ( Severity(Error), message )

-------------------------------------------------------------------------------
-- Modules                                                                   --
-------------------------------------------------------------------------------
-- | Transforms the @ghc-lib-parser@ representation of a located Haskell module
--   into the @haskell-src-transformations@ representation of a Haskell module.
transformModule :: Member Report r
                => GHC.Located (GHC.HsModule GHC.GhcPs)
                -> Sem r (S.Module GHC)
transformModule (GHC.L s modul)
  = let modName' = transformModuleName <$> GHC.hsmodName modul
    in S.Module (transformSrcSpan s)
       (OriginalModuleHead (GHC.hsmodName modul) (GHC.hsmodExports modul)
        (GHC.hsmodImports modul) (GHC.hsmodDeprecMessage modul)
        (GHC.hsmodHaddockModHeader modul)) modName'
       <$> fmap catMaybes (mapM transformImportDecl (GHC.hsmodImports modul))
       <*> mapM transformDecl (GHC.hsmodDecls modul)

-------------------------------------------------------------------------------
-- Declarations                                                              --
-------------------------------------------------------------------------------
-- | Transforms a GHC located declaration into an HST declaration.
--
--   Unsupported declarations are preserved by wrapping them in the
--   'S.OtherDecl' constructor.
transformDecl :: Member Report r => GHC.LHsDecl GHC.GhcPs -> Sem r (S.Decl GHC)

-- Data type and newtype declarations are supported.
transformDecl decl@(GHC.L s (GHC.TyClD _ dataDecl@GHC.DataDecl {})) = do
  mDataDefn <- transformDataDefn (GHC.tcdDataDefn dataDecl)
  case mDataDefn of
    Nothing       -> return $ S.OtherDecl (transformSrcSpan s) (Decl decl)
    Just dataDefn -> do
      name <- transformRdrNameUnqual (GHC.tcdLName dataDecl)
      return $ S.DataDecl (transformSrcSpan s) (Decl decl) name dataDefn
-- Function declarations are supported.
transformDecl (GHC.L s (GHC.ValD _ fb@GHC.FunBind {})) = do
  anyMatches' <- transformMatchGroup (GHC.fun_matches fb)
  matches' <- mapM funMatchToMatch anyMatches'
  return $ S.FunBind (transformSrcSpan s) matches'
 where
  -- | Completes the translation of a match for a function declaration.
  funMatchToMatch :: Member Report r => AnyMatch -> Sem r (S.Match GHC)
  funMatchToMatch (AnyMatch s' ctxt@GHC.FunRhs {} pats' rhs' mBinds')
    = case GHC.mc_strictness ctxt of
      GHC.NoSrcStrict -> do
        name' <- transformRdrNameUnqual (GHC.mc_fun ctxt)
        return
          $ S.Match { S.matchSrcSpan = s'
                    , S.matchName    = name'
                    , S.matchIsInfix = GHC.mc_fixity ctxt == GHC.Infix
                    , S.matchPats    = pats'
                    , S.matchRhs     = rhs'
                    , S.matchBinds   = mBinds'
                    }
      _               ->
        notSupported "Function declarations with strictness annotations" s'
  funMatchToMatch (AnyMatch s' _ _ _ _)
    = notSupported "Non-function matches in function declarations" s'
-- Type and data families, type declarations, type classes and extension
-- declarations are not supported and therefore skipped. The user is explicitly
-- informed about skipped type classes since they might contain pattern
-- matching.
transformDecl decl@(GHC.L s (GHC.TyClD _ GHC.FamDecl {})) = return
  $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.TyClD _ GHC.SynDecl {})) = return
  $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.TyClD _ GHC.ClassDecl {})) = do
  skipNotSupported "Type classes" (transformSrcSpan s)
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl (GHC.L _ (GHC.TyClD _ (GHC.XTyClDecl x))) = GHC.noExtCon x
-- Type class instances, data family instances, type family instances and
-- extension declarations are not supported and therefore skipped. The user is
-- explicitly informed about the first two since they might contain pattern
-- matching.
transformDecl decl@(GHC.L s (GHC.InstD _ GHC.ClsInstD {})) = do
  skipNotSupported "Type class instances" (transformSrcSpan s)
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.InstD _ GHC.DataFamInstD {})) = do
  skipNotSupported "Data family instances" (transformSrcSpan s)
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.InstD _ GHC.TyFamInstD {})) = return
  $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl (GHC.L _ (GHC.InstD _ (GHC.XInstDecl x))) = GHC.noExtCon x
-- More complex pattern bindings (pattern binds are never simple in the GHC
-- AST), abstraction bindings and pattern synonyms are not supported and
-- therefore skipped. The user is explicitly informed about this since there
-- may be errors due to this.
transformDecl decl@(GHC.L s (GHC.ValD _ GHC.PatBind {})) = do
  skipNotSupported "Non-variable pattern bindings" (transformSrcSpan s)
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.ValD _ GHC.AbsBinds {})) = do
  skipNotSupported "Abstraction bindings" (transformSrcSpan s)
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.ValD _ (GHC.PatSynBind _ _))) = do
  skipNotSupported "Pattern synonyms" (transformSrcSpan s)
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)
--  Variable bindings and extensions shouldn't occur in the AST after parsing.
transformDecl decl@(GHC.L s (GHC.ValD _ GHC.VarBind {})) = return
  $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl (GHC.L _ (GHC.ValD _ (GHC.XHsBindsLR x))) = GHC.noExtCon x
-- Template Haskell is not supported. The user is informed when
-- splices are skipped since they contain expressions that are
-- not transformed.
transformDecl decl@(GHC.L s (GHC.SpliceD _ _)) = do
  skipNotSupported "Template Haskell splicing declarations" (transformSrcSpan s)
  return $ S.OtherDecl (transformSrcSpan s) (Decl decl)
-- All other declarations are skipped silently.
transformDecl decl@(GHC.L s (GHC.DerivD _ _)) = return
  $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.SigD _ _)) = return
  $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.KindSigD _ _)) = return
  $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.DefD _ _)) = return
  $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.ForD _ _)) = return
  $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.WarningD _ _)) = return
  $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.AnnD _ _)) = return
  $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.RuleD _ _)) = return
  $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.DocD _ _)) = return
  $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl decl@(GHC.L s (GHC.RoleAnnotD _ _)) = return
  $ S.OtherDecl (transformSrcSpan s) (Decl decl)
transformDecl (GHC.L _ (GHC.XHsDecl x)) = GHC.noExtCon x

-- | Transforms a GHC located import declaration into an HST import
--   declaration.
--
--   Unsupported import declarations are skipped and the user is informed
--   about them as missing imports could lead to further errors.
transformImportDecl :: Member Report r
                    => GHC.LImportDecl GHC.GhcPs
                    -> Sem r (Maybe (S.ImportDecl GHC))
transformImportDecl (GHC.L s GHC.ImportDecl { GHC.ideclSource = True })
  = skipNotSupported "{-# SOURCE #-} imports" (transformSrcSpan s)
  >> return Nothing
transformImportDecl (GHC.L s GHC.ImportDecl { GHC.ideclSafe = True })
  = skipNotSupported "Safe imports" (transformSrcSpan s) >> return Nothing
transformImportDecl (GHC.L s GHC.ImportDecl { GHC.ideclImplicit = True })
  = skipNotSupported "Implicit imports" (transformSrcSpan s) >> return Nothing
transformImportDecl (GHC.L s GHC.ImportDecl { GHC.ideclPkgQual = Just _ })
  = skipNotSupported "Package imports" (transformSrcSpan s) >> return Nothing
transformImportDecl (GHC.L s GHC.ImportDecl { GHC.ideclHiding = Just _ })
  = skipNotSupported "Import specifications" (transformSrcSpan s)
  >> return Nothing
transformImportDecl (GHC.L s importDecl) = return
  $ Just S.ImportDecl
  { S.importSrcSpan = transformSrcSpan s
  , S.importModule  = transformModuleName (GHC.ideclName importDecl)
  , S.importIsQual  = GHC.ideclQualified importDecl /= GHC.NotQualified
  , S.importAsName  = fmap transformModuleName (GHC.ideclAs importDecl)
  }

-------------------------------------------------------------------------------
-- Data Type Declarations                                                    --
-------------------------------------------------------------------------------
-- | Transforms a GHC data definition into HST constructor declarations.
--
--   The result is wrapped inside the @Maybe@ type since some kinds of data
--   definitions are not supported by the pattern matching compiler and are
--   therefore skipped. @Nothing@ is returned if the data definition itself or
--   any of its constructors is not supported.
transformDataDefn :: Member Report r
                  => GHC.HsDataDefn GHC.GhcPs
                  -> Sem r (Maybe [S.ConDecl GHC])
transformDataDefn GHC.HsDataDefn { GHC.dd_cons = cons } = do
  conDecls <- mapM transformConDecl cons
  return $ if all isJust conDecls then Just (catMaybes conDecls) else Nothing
transformDataDefn (GHC.XHsDataDefn x)                   = GHC.noExtCon x

-- | Transforms a GHC located constructor declaration into an HST constructor
--   declaration.
--
--   The result is wrapped inside the @Maybe@ type since some kinds of
--   constructors are not supported by the pattern matching compiler in which
--   case the corresponding data definition is skipped.
transformConDecl
  :: Member Report r => GHC.LConDecl GHC.GhcPs -> Sem r (Maybe (S.ConDecl GHC))
transformConDecl (GHC.L s conDecl@GHC.ConDeclH98 {}) = do
  name <- transformRdrNameUnqual (GHC.con_name conDecl)
  transformConDetails (transformSrcSpan s) name (GHC.con_args conDecl)
transformConDecl (GHC.L s GHC.ConDeclGADT {})        = do
  skipNotSupported "GADT constructors" (transformSrcSpan s)
  return Nothing
transformConDecl (GHC.L _ (GHC.XConDecl x))          = GHC.noExtCon x

-- | Transforms an HST constructor name and GHC constructor details into an HST
--   constructor declaration.
--
--   The result is wrapped inside the @Maybe@ type since some kinds of
--   constructors are not supported by the pattern matching compiler in which
--   case the corresponding data definition is skipped.
transformConDetails :: Member Report r
                    => S.SrcSpan GHC
                    -> S.Name GHC
                    -> GHC.HsConDetails (GHC.LBangType GHC.GhcPs) recType
                    -> Sem r (Maybe (S.ConDecl GHC))
transformConDetails s name (GHC.PrefixCon args) = return
  $ Just S.ConDecl { S.conDeclSrcSpan = s
                   , S.conDeclName    = name
                   , S.conDeclArity   = length args
                   , S.conDeclIsInfix = False
                   }
transformConDetails s name (GHC.InfixCon _ _)   = return
  $ Just S.ConDecl { S.conDeclSrcSpan = s
                   , S.conDeclName    = name
                   , S.conDeclArity   = 2
                   , S.conDeclIsInfix = True
                   }
-- TODO Maybe use a Symbol instead of an Ident name for InfixCon (does that make a difference?)
transformConDetails s _ (GHC.RecCon _)          = do
  skipNotSupported "Record constructors" s
  return Nothing

-------------------------------------------------------------------------------
-- Function Declarations                                                     --
-------------------------------------------------------------------------------
-- | Transforms a GHC located binding group into an HST binding group.
transformLocalBinds :: Member Report r
                    => GHC.LHsLocalBinds GHC.GhcPs
                    -> Sem r (Maybe (S.Binds GHC))
transformLocalBinds (GHC.L s (GHC.HsValBinds _ binds)) = do
  binds' <- transformValBinds (transformSrcSpan s) binds
  return $ Just (S.BDecls (transformSrcSpan s) binds')
transformLocalBinds (GHC.L _ (GHC.EmptyLocalBinds _))  = return Nothing
transformLocalBinds (GHC.L s (GHC.HsIPBinds _ _))      = notSupported
  "Implicit-parameters" (transformSrcSpan s)
transformLocalBinds (GHC.L _ (GHC.XHsLocalBindsLR x))  = GHC.noExtCon x

-- | Transforms GHC value bindings into HST declarations.
transformValBinds :: Member Report r
                  => S.SrcSpan a
                  -> GHC.HsValBinds GHC.GhcPs
                  -> Sem r [S.Decl GHC]
transformValBinds _ (GHC.ValBinds _ binds sigs) = mapM transformDecl
  (map (\(GHC.L s bind) -> GHC.L s (GHC.ValD GHC.NoExtField bind))
   (GHC.bagToList binds)
   ++ map (\(GHC.L s sig) -> GHC.L s (GHC.SigD GHC.NoExtField sig)) sigs)
transformValBinds s (GHC.XValBindsLR _)
  = notSupported "Value bindings extensions" s

-- | Transforms a GHC match group into HST matches without transforming the
--   match context.
transformMatchGroup :: Member Report r
                    => GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
                    -> Sem r [AnyMatch]
transformMatchGroup GHC.MG { GHC.mg_alts = GHC.L _ matches }
  = mapM transformMatch matches
transformMatchGroup (GHC.XMatchGroup x) = GHC.noExtCon x

-- | Transforms a GHC located match into an HST match without transforming the
--   match context.
transformMatch :: Member Report r
               => GHC.LMatch GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
               -> Sem r AnyMatch
transformMatch (GHC.L s match@GHC.Match {}) = do
  pats <- mapM transformPat (GHC.m_pats match)
  (rhs, mBinds) <- transformGRHSs (GHC.m_grhss match)
  return AnyMatch { anyMatchSrcSpan  = transformSrcSpan s
                  , anyMatchContext  = GHC.m_ctxt match
                  , anyMatchPatterns = pats
                  , anyMatchRhs      = rhs
                  , anyMatchBinds    = mBinds
                  }
transformMatch (GHC.L _ (GHC.XMatch x))     = GHC.noExtCon x

-- | Transforms GHC guarded right-hand sides into an HST right-hand side and
--   binding group.
transformGRHSs :: Member Report r
               => GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
               -> Sem r (S.Rhs GHC, Maybe (S.Binds GHC))
transformGRHSs grhss@GHC.GRHSs {} = do
  binds <- transformLocalBinds (GHC.grhssLocalBinds grhss)
  case GHC.grhssGRHSs grhss of
    [GHC.L s (GHC.GRHS _ [] body)] -> do
      body' <- transformExpr body
      return (S.UnGuardedRhs (transformSrcSpan s) body', binds)
    grhss' -> do
      grhss'' <- mapM transformGRHS grhss'
      let srcSpan = combineSrcSpans (map S.getSrcSpan grhss'')
      return (S.GuardedRhss srcSpan grhss'', binds)
transformGRHSs (GHC.XGRHSs x)     = GHC.noExtCon x

-- | Transforms a GHC guarded right-hand side into an HST guarded right-hand
--   side.
transformGRHS :: Member Report r
              => GHC.LGRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
              -> Sem r (S.GuardedRhs GHC)
transformGRHS (GHC.L s (GHC.GRHS _ [gStmt] body))
  = S.GuardedRhs (transformSrcSpan s) <$> transformStmtExpr gStmt
  <*> transformExpr body
transformGRHS (GHC.L s (GHC.GRHS _ _ _))
  = notSupported "Guarded right-hand sides without exactly one guard statement"
  (transformSrcSpan s)
transformGRHS (GHC.L _ (GHC.XGRHS x))             = GHC.noExtCon x

-- | Transforms a GHC located statement consisting only of a single expression
--   into an HST expression.
transformStmtExpr :: Member Report r
                  => GHC.LStmt GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
                  -> Sem r (S.Exp GHC)
transformStmtExpr (GHC.L _ (GHC.BodyStmt _ body _ _))   = transformExpr body
-- TODO Are there more statements that can be safely converted to boolean expressions?
transformStmtExpr (GHC.L s (GHC.LastStmt _ _ _ _))      = notSupported
  "Last statements in guards" (transformSrcSpan s)
transformStmtExpr (GHC.L s (GHC.BindStmt _ _ _ _ _))    = notSupported
  "Bind statements in guards" (transformSrcSpan s)
transformStmtExpr (GHC.L s (GHC.ApplicativeStmt _ _ _)) = notSupported
  "Applicative statements in guards" (transformSrcSpan s)
transformStmtExpr (GHC.L s (GHC.LetStmt _ _))           = notSupported
  "Let statements in guards" (transformSrcSpan s)
transformStmtExpr (GHC.L s (GHC.ParStmt _ _ _ _))       = notSupported
  "Parenthesised statements in guards" (transformSrcSpan s)
transformStmtExpr (GHC.L s GHC.TransStmt {})            = notSupported
  "Transform statements in guards" (transformSrcSpan s)
transformStmtExpr (GHC.L s GHC.RecStmt {})              = notSupported
  "Recursive statements in guards" (transformSrcSpan s)
transformStmtExpr (GHC.L _ (GHC.XStmtLR x))             = GHC.noExtCon x

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
    (qName, True)  -> S.Con s' qName
transformExpr (GHC.L s (GHC.HsUnboundVar _ _))
  = let s' = transformSrcSpan s
    in return $ S.Var s' (S.Special s' (S.ExprHole s'))
transformExpr (GHC.L s (GHC.HsLit _ lit)) = return
  $ S.Lit (transformSrcSpan s) (Lit lit)
transformExpr (GHC.L s (GHC.HsOverLit _ lit)) = return
  $ S.Lit (transformSrcSpan s) (OverLit lit)
transformExpr (GHC.L s (GHC.OpApp _ e1 op e2)) = do
  e1' <- transformExpr e1
  e2' <- transformExpr e2
  op' <- transformExpr op
  op'' <- case op' of
    (S.Var s' name) -> return $ S.QVarOp s' name
    (S.Con s' name) -> return $ S.QConOp s' name
    opExp           ->
      notSupported "Infix operators that aren't variables or constructors"
      (S.getSrcSpan opExp)
  return $ S.InfixApp (transformSrcSpan s) e1' op'' e2'
transformExpr (GHC.L s (GHC.HsApp _ e1 e2))
  = S.App (transformSrcSpan s) <$> transformExpr e1 <*> transformExpr e2
transformExpr (GHC.L s (GHC.NegApp _ e _)) = S.NegApp (transformSrcSpan s)
  <$> transformExpr e
transformExpr (GHC.L s (GHC.HsLam _ mg)) = do
  let s' = transformSrcSpan s
  mg' <- transformMatchGroup mg
  case mg' of
    [ AnyMatch _ GHC.LambdaExpr pats' (S.UnGuardedRhs _ e') Nothing
      ] -> return $ S.Lambda s' pats' e'
    [ AnyMatch _ GHC.LambdaExpr _ _ (Just _)
      ] -> notSupported "Lambda abstractions with bindings" s'
    [ AnyMatch _ GHC.LambdaExpr _ (S.GuardedRhss _ _) _
      ] -> notSupported "Lambda abstractions with guards" s'
    [] -> notSupported "Empty lambda abstractions" s'
    [_] -> notSupported "Non-lambda matches in lambda expressions" s'
    _ -> notSupported "Lambda abstractions with multiple matches" s'
transformExpr (GHC.L s (GHC.HsLet _ binds e)) = do
  mBinds <- transformLocalBinds binds
  case mBinds of
    Nothing     -> notSupported "Let expressions with empty bindings"
      (transformSrcSpan s)
    Just binds' -> S.Let (transformSrcSpan s) binds' <$> transformExpr e
transformExpr (GHC.L s (GHC.HsIf _ _ e1 e2 e3)) = S.If (transformSrcSpan s)
  <$> transformExpr e1
  <*> transformExpr e2
  <*> transformExpr e3
transformExpr (GHC.L s (GHC.HsCase _ e mg)) = do
  e' <- transformExpr e
  mg' <- transformMatchGroup mg
  alts <- mapM matchToAlt mg'
  return $ S.Case (transformSrcSpan s) e' alts
 where
  -- | Completes the transformation of a match to an alternative.
  matchToAlt :: Member Report r => AnyMatch -> Sem r (S.Alt GHC)
  matchToAlt (AnyMatch s' GHC.CaseAlt [pat'] rhs' mBinds')
    = return $ S.Alt s' pat' rhs' mBinds'
  matchToAlt (AnyMatch s' GHC.CaseAlt [] _ _)
    = notSupported "Case alternatives without patterns" s'
  matchToAlt (AnyMatch s' GHC.CaseAlt _ _ _)
    = notSupported "Case alternatives with multiple patterns" s'
  matchToAlt (AnyMatch s' _ _ _ _)
    = notSupported "Non-case expression matches in case expressions" s'
transformExpr (GHC.L s (GHC.ExplicitTuple _ tArgs boxity)) = S.Tuple
  (transformSrcSpan s) (transformBoxity boxity)
  <$> mapM transformTupleArg tArgs
transformExpr (GHC.L s (GHC.ExplicitList _ _ es)) = S.List (transformSrcSpan s)
  <$> mapM transformExpr es
transformExpr (GHC.L s (GHC.HsPar _ e)) = S.Paren (transformSrcSpan s)
  <$> transformExpr e
transformExpr (GHC.L s (GHC.ExprWithTySig _ e typeSig)) = do
  e' <- transformExpr e
  return $ S.ExpTypeSig (transformSrcSpan s) e' (SigType typeSig)
-- All other expressions are not supported.
transformExpr (GHC.L s (GHC.HsConLikeOut _ _)) = notSupported
  "Expressions introduced by the type checker" (transformSrcSpan s)
transformExpr (GHC.L s (GHC.HsRecFld _ _)) = notSupported "Records"
  (transformSrcSpan s)
transformExpr (GHC.L s (GHC.HsOverLabel _ _ _)) = notSupported
  "Overloaded labels" (transformSrcSpan s)
transformExpr (GHC.L s (GHC.HsIPVar _ _)) = notSupported "Implicit parameters"
  (transformSrcSpan s)
transformExpr (GHC.L s (GHC.HsLamCase _ _)) = notSupported
  "Lambda-case-expressions" (transformSrcSpan s)
transformExpr (GHC.L s (GHC.HsAppType _ _ _)) = notSupported
  "Visible type applications" (transformSrcSpan s)
transformExpr (GHC.L s (GHC.SectionL _ _ _)) = notSupported "Sections"
  (transformSrcSpan s)
transformExpr (GHC.L s (GHC.SectionR _ _ _)) = notSupported "Sections"
  (transformSrcSpan s)
transformExpr (GHC.L s (GHC.ExplicitSum _ _ _ _)) = notSupported "Unboxed sums"
  (transformSrcSpan s)
transformExpr (GHC.L s (GHC.HsMultiIf _ _)) = notSupported
  "Multi-way if-expressions" (transformSrcSpan s)
transformExpr (GHC.L s (GHC.HsDo _ _ _)) = notSupported "do-expressions"
  (transformSrcSpan s)
transformExpr (GHC.L s GHC.RecordCon {}) = notSupported "Records"
  (transformSrcSpan s)
transformExpr (GHC.L s GHC.RecordUpd {}) = notSupported "Records"
  (transformSrcSpan s)
transformExpr (GHC.L s (GHC.ArithSeq _ _ _)) = notSupported
  "Arithmetic sequences" (transformSrcSpan s)
transformExpr (GHC.L s (GHC.HsSCC _ _ _ _)) = notSupported
  "Set-cost-centre-expressions" (transformSrcSpan s)
transformExpr (GHC.L s (GHC.HsCoreAnn _ _ _ _)) = notSupported
  "Core annotations" (transformSrcSpan s)
transformExpr (GHC.L s (GHC.HsBracket _ _)) = notSupported
  "Template Haskell expressions" (transformSrcSpan s)
transformExpr (GHC.L s (GHC.HsRnBracketOut _ _ _)) = notSupported
  "Template Haskell expressions" (transformSrcSpan s)
transformExpr (GHC.L s (GHC.HsTcBracketOut _ _ _)) = notSupported
  "Template Haskell expressions" (transformSrcSpan s)
transformExpr (GHC.L s (GHC.HsSpliceE _ _)) = notSupported
  "Template Haskell expressions" (transformSrcSpan s)
transformExpr (GHC.L s (GHC.HsProc _ _ _)) = notSupported "Arrow expressions"
  (transformSrcSpan s)
transformExpr (GHC.L s (GHC.HsStatic _ _)) = notSupported "Static pointers"
  (transformSrcSpan s)
transformExpr (GHC.L s (GHC.HsTick _ _ _)) = notSupported
  "Haskell program coverage" (transformSrcSpan s)
transformExpr (GHC.L s (GHC.HsBinTick _ _ _ _)) = notSupported
  "Haskell program coverage" (transformSrcSpan s)
transformExpr (GHC.L s (GHC.HsTickPragma _ _ _ _ _)) = notSupported
  "Haskell program coverage" (transformSrcSpan s)
transformExpr (GHC.L s (GHC.HsWrap _ _ _)) = notSupported
  "Expressions introduced by the type checker" (transformSrcSpan s)
transformExpr (GHC.L _ (GHC.XExpr x)) = GHC.noExtCon x

-- | Transforms a GHC located tuple argument consisting of an expression into
--   an HST expression.
transformTupleArg
  :: Member Report r => GHC.LHsTupArg GHC.GhcPs -> Sem r (S.Exp GHC)
transformTupleArg (GHC.L _ (GHC.Present _ e)) = transformExpr e
transformTupleArg (GHC.L s (GHC.Missing _))   = notSupported
  "Missing tuple arguments" (transformSrcSpan s)
transformTupleArg (GHC.L _ (GHC.XTupArg x))   = GHC.noExtCon x

-------------------------------------------------------------------------------
-- Patterns                                                                  --
-------------------------------------------------------------------------------
-- | Transforms a GHC located pattern into an HST pattern.
transformPat :: Member Report r => GHC.LPat GHC.GhcPs -> Sem r (S.Pat GHC)
transformPat (GHC.L s (GHC.VarPat _ name))
  = S.PVar (transformSrcSpan s) <$> transformRdrNameUnqual name
transformPat (GHC.L s (GHC.ConPatIn name cpds))     = do
  let s' = transformSrcSpan s
  (name', isCon) <- transformRdrName name
  case (cpds, isCon) of
    (GHC.InfixCon pat1 pat2, True) -> S.PInfixApp s' <$> transformPat pat1
      <*> return name'
      <*> transformPat pat2
    (GHC.PrefixCon pats, True) -> S.PApp s' name' <$> mapM transformPat pats
    (_, True) -> notSupported "Record constructors" s'
    _ -> notSupported "Non-constructor applications in patterns" s'
-- TODO The documentation also mentions a more complicated ConPatOut.
-- Do we need to consider that?
transformPat (GHC.L s (GHC.TuplePat _ pats boxity)) = S.PTuple
  (transformSrcSpan s) (transformBoxity boxity)
  <$> mapM transformPat pats
transformPat (GHC.L s (GHC.ParPat _ pat))
  = S.PParen (transformSrcSpan s) <$> transformPat pat
transformPat (GHC.L s (GHC.ListPat _ pats))
  = S.PList (transformSrcSpan s) <$> mapM transformPat pats
transformPat (GHC.L s (GHC.WildPat _))              = return
  $ S.PWildCard (transformSrcSpan s)
-- All other patterns are not supported.
transformPat (GHC.L s (GHC.LazyPat _ _))            = notSupported
  "Lazy patterns" (transformSrcSpan s)
transformPat (GHC.L s (GHC.AsPat _ _ _))            = notSupported "as-patterns"
  (transformSrcSpan s)
transformPat (GHC.L s (GHC.BangPat _ _))            = notSupported
  "Bang patterns" (transformSrcSpan s)
transformPat (GHC.L s (GHC.SumPat _ _ _ _))         = notSupported
  "Anonymous sum patterns" (transformSrcSpan s)
transformPat (GHC.L s GHC.ConPatOut {})             = notSupported
  "Constructor patterns out" (transformSrcSpan s)
transformPat (GHC.L s (GHC.ViewPat _ _ _))          = notSupported
  "View patterns" (transformSrcSpan s)
transformPat (GHC.L s (GHC.SplicePat _ _))          = notSupported
  "Template Haskell" (transformSrcSpan s)
transformPat (GHC.L s (GHC.LitPat _ _))             = notSupported
  "Literal patterns" (transformSrcSpan s)
transformPat (GHC.L s (GHC.NPat _ _ _ _))           = notSupported
  "Natural patterns" (transformSrcSpan s)
transformPat (GHC.L s (GHC.NPlusKPat _ _ _ _ _ _))  = notSupported
  "n+k patterns" (transformSrcSpan s)
transformPat (GHC.L s (GHC.SigPat _ _ _))           = notSupported
  "Patterns with type signature" (transformSrcSpan s)
transformPat (GHC.L s (GHC.CoPat _ _ _ _))          = notSupported
  "Coercion patterns" (transformSrcSpan s)
transformPat (GHC.L _ (GHC.XPat x))                 = GHC.noExtCon x

-------------------------------------------------------------------------------
-- Names                                                                     --
-------------------------------------------------------------------------------
-- | Transforms a GHC module name with an HST source span into an HST module
--   name.
transformModuleName :: GHC.Located GHC.ModuleName -> S.ModuleName GHC
transformModuleName (GHC.L s modName) = S.ModuleName (transformSrcSpan s)
  (GHC.moduleNameString modName)

-- | Transforms a GHC located reader name into an HST qualified name and a
--   @Bool@ which is @True@ if the name belongs to a data constructor and
--   @False@ otherwise.
transformRdrName
  :: Member Report r => GHC.Located GHC.RdrName -> Sem r (S.QName GHC, Bool)
transformRdrName (GHC.L s (GHC.Unqual name))
  = let s' = transformSrcSpan s
    in return
       (S.UnQual s' (S.Ident s' (GHC.occNameString name)), GHC.isDataOcc name)
transformRdrName (GHC.L s (GHC.Qual modName name))
  = let s' = transformSrcSpan s
    in return ( S.Qual s' (transformModuleName (GHC.L s modName))
                  (S.Ident s' (GHC.occNameString name))
              , GHC.isDataOcc name
              )
transformRdrName (GHC.L s (GHC.Exact name))        = do
  let s' = transformSrcSpan s
  specialCon <- transformSpecialCon s' name
  return (S.Special s' specialCon, True)
transformRdrName (GHC.L s (GHC.Orig _ _))          = notSupported
  "Original names" (transformSrcSpan s)

-- | Transforms a GHC located unqualified reader name into an HST name.
transformRdrNameUnqual
  :: Member Report r => GHC.Located GHC.RdrName -> Sem r (S.Name GHC)
transformRdrNameUnqual (GHC.L s (GHC.Unqual occName)) = return
  $ S.Ident (transformSrcSpan s) (GHC.occNameString occName)
transformRdrNameUnqual (GHC.L s (GHC.Qual _ _))       = notSupported
  "Qualified names where unqualified names are expected" (transformSrcSpan s)
transformRdrNameUnqual (GHC.L s (GHC.Orig _ _))       = notSupported
  "Original names" (transformSrcSpan s)
transformRdrNameUnqual (GHC.L s (GHC.Exact _))        = notSupported
  "Exact names where unqualified names are expected" (transformSrcSpan s)

-- | Transforms a GHC name with an HST source span into an HST special
--   constructor.
transformSpecialCon
  :: Member Report r => S.SrcSpan GHC -> GHC.Name -> Sem r (S.SpecialCon GHC)
transformSpecialCon s name = case Map.lookup name specialDataConMap of
  Just mkSpecialCon -> return $ mkSpecialCon s
  Nothing           -> case GHC.wiredInNameTyThing_maybe name of
    Just (GHC.AConLike (GHC.RealDataCon dataCon))
      | GHC.isUnboxedTupleCon dataCon ->
        return $ S.TupleCon s S.Unboxed $ GHC.dataConSourceArity dataCon
      | GHC.isTupleDataCon dataCon ->
        return $ S.TupleCon s S.Boxed $ GHC.dataConSourceArity dataCon
    _ -> reportFatal
      $ message Error s
      $ ("Wired-in data constructor not supported: "
         ++ GHC.occNameString (GHC.nameOccName name))

-- | Maps GHC names of data constructors to functions that build HST special
--   constructor nodes with the given source span.
--
--   Tuple constructors cannot be transformed with this map and are instead
--   transformed directly in 'transformSpecialCon'.
--   Expression holes appear at expression level in the GHC AST and are
--   transformed in 'transformExpr' instead.
specialDataConMap :: Map GHC.Name (S.SrcSpan GHC -> S.SpecialCon GHC)
specialDataConMap = Map.fromList
  [ (GHC.dataConName GHC.unitDataCon, S.UnitCon)
  , (GHC.dataConName GHC.nilDataCon, S.NilCon)
  , (GHC.dataConName GHC.consDataCon, S.ConsCon)
  , (GHC.dataConName GHC.unboxedUnitDataCon, S.UnboxedSingleCon)
  ]

-------------------------------------------------------------------------------
-- Source Spans                                                              --
-------------------------------------------------------------------------------
-- | Wraps a GHC source span into the HST type for source spans.
transformSrcSpan :: GHC.SrcSpan -> S.SrcSpan GHC
transformSrcSpan srcSpan@(GHC.RealSrcSpan realSrcSpan) = S.SrcSpan srcSpan
  S.MsgSrcSpan
  { S.msgSrcSpanFilePath    = GHC.unpackFS (GHC.srcSpanFile realSrcSpan)
  , S.msgSrcSpanStartLine   = GHC.srcSpanStartLine realSrcSpan
  , S.msgSrcSpanStartColumn = GHC.srcSpanStartCol realSrcSpan
  , S.msgSrcSpanEndLine     = GHC.srcSpanEndLine realSrcSpan
  , S.msgSrcSpanEndColumn   = GHC.srcSpanEndCol realSrcSpan
  }
transformSrcSpan (GHC.UnhelpfulSpan _)                 = S.NoSrcSpan

-- | Combines multiple source spans into a source span that spans at least
--   all characters within the individual spans.
combineSrcSpans :: [S.SrcSpan GHC] -> S.SrcSpan GHC
combineSrcSpans = transformSrcSpan
  . foldr (GHC.combineSrcSpans . S.originalSrcSpan) GHC.noSrcSpan
