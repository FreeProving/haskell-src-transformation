{-# LANGUAGE PackageImports, TypeFamilies #-}

-- | This module contains functions transforming Haskell modules and other
--   constructs of the AST data structure of @ghc-lib-parser@ into the
--   corresponding constructs of the AST data structure in the
--   "HST.Frontend.Syntax" module.

module HST.Frontend.FromGHC where

import           Data.Data                      ( Data )
import           Data.Maybe                     ( fromMaybe
                                                , mapMaybe
                                                )

import qualified "ghc-lib-parser" GHC.Hs       as GHC
import qualified "ghc-lib-parser" SrcLoc       as GHC
import qualified "ghc-lib-parser" RdrName      as GHC
import qualified "ghc-lib-parser" OccName      as GHC
import qualified "ghc-lib-parser" BasicTypes   as GHC
import qualified "ghc-lib-parser" Bag          as GHC
import qualified "ghc-lib-parser" TysWiredIn   as GHC
import qualified "ghc-lib-parser" Module       as GHC
import qualified "ghc-lib-parser" Name         as GHC
import qualified "ghc-lib-parser" TyCon        as GHC
import qualified "ghc-lib-parser" Type         as GHC
import qualified "ghc-lib-parser" DynFlags     as GHC
import qualified "ghc-lib-parser" GHC.Hs.Dump  as GHC
import qualified "ghc-lib-parser" Outputable   as GHC
import qualified Language.Haskell.GhclibParserEx.GHC.Settings.Config
                                               as GHC

import qualified HST.Frontend.Syntax           as S

-------------------------------------------------------------------------------
-- Type Family Instances                                                     --
-------------------------------------------------------------------------------

-- | Type representing the AST data structure of @ghc-lib-parser@.
--
--   Instantiates the type families for source spans, literals and type
--   expressions with the concrete types from @ghc-lib-parser@ or wrappers for
--   these types. Also adds instances for 'S.EqAST' and 'S.ShowAST' to allow
--   the usage of @==@ and @show@.
data GHC
type instance S.SrcSpanType GHC = GHC.SrcSpan
type instance S.Literal GHC = LitWrapper
type instance S.TypeExp GHC = TypeWrapper

instance S.EqAST GHC
instance S.ShowAST GHC

-------------------------------------------------------------------------------
-- Wrappers for @ghc-lib-parser@ Types                                       --
-------------------------------------------------------------------------------

-- | Wrapper for the two literal types (for regular and overloaded literals)
--   used by @ghc-lib-parser@.
data LitWrapper = Lit (GHC.HsLit GHC.GhcPs)
                | OverLit (GHC.HsOverLit GHC.GhcPs)
  deriving Eq

instance Show LitWrapper where
  show (Lit     l) = defaultPrintShow l
  show (OverLit l) = defaultPrintShow l

-- | Wrapper for the type for type expressions appearing in type signatures
--   used by @ghc-lib-parser@.
newtype TypeWrapper = SigType (GHC.LHsSigWcType GHC.GhcPs)

instance Eq TypeWrapper where
  SigType t1 == SigType t2 = defaultPrintEq t1 == defaultPrintEq t2

instance Show TypeWrapper where
  show (SigType t) = defaultPrintShow t

-------------------------------------------------------------------------------
-- Printing Functions for the @ghc-lib-parser@ AST                           --
-------------------------------------------------------------------------------

-- | Prints a component of the @ghc-lib-parser@ AST data structure without
--   source span information and with line breaks.
--
--   This function is used for equality of @ghc-lib-parser@ AST components.
defaultPrintEq :: Data a => a -> String
defaultPrintEq d =
  GHC.showSDoc defaultDynFlags (GHC.showAstData GHC.BlankSrcSpan d)

-- | Prints a component of the @ghc-lib-parser@ AST data structure with source
--   span information and without line breaks.
--
--   This function is used for showing @ghc-lib-parser@ AST components.
defaultPrintShow :: Data a => a -> String
defaultPrintShow d =
  GHC.showSDocOneLine defaultDynFlags (GHC.showAstData GHC.NoBlankSrcSpan d)

-- | Returns default variants of the dynamic flags used by @ghc-lib-parser@.
--
--   These dynamic flags are used for parsing Haskell code and printing AST
--   components.
defaultDynFlags :: GHC.DynFlags
-- TODO These DynFlags should be the same as the ones used for parsing Haskell
-- modules and should be defined in another module.
defaultDynFlags = GHC.defaultDynFlags GHC.fakeSettings GHC.fakeLlvmConfig

-------------------------------------------------------------------------------
-- Transformation Functions                                                  --
-------------------------------------------------------------------------------

-- | Transforms the @ghc-lib-parser@ representation of a Haskell module into
--   the @haskell-src-transformations@ representation of a Haskell module.
transformModule :: GHC.HsModule GHC.GhcPs -> S.Module GHC
transformModule modul =
  let modName' = case GHC.hsmodName modul of
        Just (GHC.L s modName) ->
          Just (transformModuleName (transformSrcSpan s) modName)
        Nothing -> Nothing
  in  S.Module modName' (mapMaybe transformDecl (GHC.hsmodDecls modul))

-- | Transforms a located GHC declaration into an HST declaration.
--
--   Unlike the other transforming functions, the result is wrapped inside
--   the @Maybe@ type, so instead of an error, @Nothing@ is returned if the
--   GHC declaration cannot be transformed.
transformDecl :: GHC.LHsDecl GHC.GhcPs -> Maybe (S.Decl GHC)
transformDecl (GHC.L _ (GHC.TyClD _ dDecl@GHC.DataDecl{})) = Just
  (S.DataDecl (transformRdrNameUnqual (GHC.tcdLName dDecl))
              (transformDataDefn (GHC.tcdDataDefn dDecl))
  )
transformDecl (GHC.L s (GHC.ValD _ fb@GHC.FunBind{})) =
  Just
    (S.FunBind (transformSrcSpan s) (transformMatchGroup (GHC.fun_matches fb)))
transformDecl (GHC.L s (GHC.SigD _ (GHC.TypeSig _ names sigType))) = Just
  (S.TypeSig (transformSrcSpan s)
             (map transformRdrNameUnqual names)
             (SigType sigType)
  )
transformDecl _ = Nothing

-- | Transforms a GHC data definition into HST constructor declarations.
transformDataDefn :: GHC.HsDataDefn GHC.GhcPs -> [S.ConDecl GHC]
transformDataDefn GHC.HsDataDefn { GHC.dd_cons = cons } =
  map transformConDecl cons
transformDataDefn _ = error "Unsupported data definition"

-- | Transforms a located GHC constructor declaration into an HST constructor
--   declaration.
transformConDecl :: GHC.LConDecl GHC.GhcPs -> S.ConDecl GHC
transformConDecl (GHC.L _ conDecl@GHC.ConDeclH98{}) = transformConDetails
  (transformRdrNameUnqual (GHC.con_name conDecl))
  (GHC.con_args conDecl)
transformConDecl _ = error "GADT constructors are not supported"

-- | Transforms an HST constructor name and GHC constructor details into an HST
--   constructor declaration.
transformConDetails
  :: S.Name GHC
  -> GHC.HsConDetails (GHC.LBangType GHC.GhcPs) recType
  -> S.ConDecl GHC
transformConDetails name (GHC.PrefixCon args) = S.ConDecl name (length args)
transformConDetails name (GHC.InfixCon _ _) = S.InfixConDecl name
-- TODO Maybe use a Symbol instead of an Ident name for InfixCon (does that make a difference?)
transformConDetails _ _ = error "Record constructors are not supported"

-- | Transforms a GHC located binding group into an HST binding group.
transformLocalBinds :: GHC.LHsLocalBinds GHC.GhcPs -> Maybe (S.Binds GHC)
transformLocalBinds (GHC.L s (GHC.HsValBinds _ binds)) =
  Just (S.BDecls (transformSrcSpan s) (transformValBinds binds))
transformLocalBinds (GHC.L _ (GHC.EmptyLocalBinds _)) = Nothing
transformLocalBinds _ = error "Unsupported local bindings"

-- | Transforms GHC value bindings into HST declarations.
transformValBinds :: GHC.HsValBinds GHC.GhcPs -> [S.Decl GHC]
transformValBinds (GHC.ValBinds _ binds sigs) = map
  (fromMaybe (error "Unsupported declaration in bindings") . transformDecl)
  (  map (\(GHC.L s bind) -> GHC.L s (GHC.ValD GHC.NoExtField bind))
         (GHC.bagToList binds)
  ++ map (\(GHC.L s sig) -> GHC.L s (GHC.SigD GHC.NoExtField sig)) sigs
  )
transformValBinds _ = error "Unsupported value bindings"

-- | Transforms a GHC match group into HST matches.
transformMatchGroup
  :: GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> [S.Match GHC]
transformMatchGroup GHC.MG { GHC.mg_alts = GHC.L _ matches } =
  map transformMatch matches
transformMatchGroup _ = error "Unsupported match group"

-- | Transforms a GHC located match into an HST match.
transformMatch :: GHC.LMatch GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> S.Match GHC
transformMatch (GHC.L s match@GHC.Match { GHC.m_ctxt = ctxt@GHC.FunRhs{} }) =
  let s'            = transformSrcSpan s
      name          = transformRdrNameUnqual (GHC.mc_fun ctxt)
      pats          = map transformPat (GHC.m_pats match)
      (rhs, mBinds) = transformGRHSs (GHC.m_grhss match)
  in  case GHC.mc_fixity ctxt of
        GHC.Prefix -> S.Match s' name pats rhs mBinds
        GHC.Infix  -> S.InfixMatch s' (head pats) name (tail pats) rhs mBinds
transformMatch _ = error "Unsupported match"

-- | Transforms GHC guarded right-hand sides into an HST right-hand side and
--   binding group.
transformGRHSs
  :: GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
  -> (S.Rhs GHC, Maybe (S.Binds GHC))
transformGRHSs grhss@GHC.GRHSs{} =
  let binds = transformLocalBinds (GHC.grhssLocalBinds grhss)
  in  case GHC.grhssGRHSs grhss of
        [GHC.L s (GHC.GRHS _ [] body)] ->
          (S.UnGuardedRhs (transformSrcSpan s) (transformExpr body), binds)
        grhss' -> (S.GuardedRhss S.NoSrcSpan (map transformGRHS grhss'), binds)
        -- The source span here seems to be missing in the GHC AST
transformGRHSs _ = error "Unsupported guarded right-hand sides"

-- | Transforms a GHC guarded right-hand side into an HST guarded right-hand
--   side.
transformGRHS :: GHC.LGRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> S.GuardedRhs GHC
transformGRHS (GHC.L s (GHC.GRHS _ [gStmt] body)) = S.GuardedRhs
  (transformSrcSpan s)
  (transformStmtExpr gStmt)
  (transformExpr body)
transformGRHS _ = error "Unsupported guarded right-hand side"

-- | Transforms a GHC located statement consisting only of a single expression
--   into an HST expression.
transformStmtExpr :: GHC.LStmt GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> S.Exp GHC
transformStmtExpr (GHC.L _ (GHC.BodyStmt _ body _ _)) = transformExpr body
transformStmtExpr _ =
  error "Only boolean expressions are supported as statements"
-- TODO Are there more statements that can be safely converted to boolean expressions?

-- | Transforms a GHC boxity into an HST boxed mark.
transformBoxity :: GHC.Boxity -> S.Boxed
transformBoxity GHC.Boxed   = S.Boxed
transformBoxity GHC.Unboxed = S.Unboxed

-- | Transforms a GHC located expression into an HST expression.
transformExpr :: GHC.LHsExpr GHC.GhcPs -> S.Exp GHC
transformExpr (GHC.L s (GHC.HsVar _ name)) =
  let s' = transformSrcSpan s
  in  case transformRdrName name of
        (qName, False) -> S.Var s' qName
        (qName, True ) -> S.Con s' qName
transformExpr (GHC.L s (GHC.HsUnboundVar _ _)) =
  let s' = transformSrcSpan s in S.Var s' (S.Special s' (S.ExprHole s'))
transformExpr (GHC.L s (GHC.HsLit _ lit)) =
  S.Lit (transformSrcSpan s) (Lit lit)
transformExpr (GHC.L s (GHC.HsOverLit _ lit)) =
  S.Lit (transformSrcSpan s) (OverLit lit)
transformExpr (GHC.L s (GHC.OpApp _ e1 op e2)) =
  let e1' = transformExpr e1
      e2' = transformExpr e2
      op' = case transformExpr op of
        (S.Var s' name) -> S.QVarOp s' name
        (S.Con s' name) -> S.QConOp s' name
        _               -> error "Unsupported operator in infix application"
  in  S.InfixApp (transformSrcSpan s) e1' op' e2'
transformExpr (GHC.L s (GHC.HsApp _ e1 e2)) =
  S.App (transformSrcSpan s) (transformExpr e1) (transformExpr e2)
transformExpr (GHC.L s (GHC.NegApp _ e _)) =
  S.NegApp (transformSrcSpan s) (transformExpr e)
transformExpr (GHC.L s (GHC.HsLam _ mg)) = case transformMatchGroup mg of
  [S.Match _ _ pats (S.UnGuardedRhs _ e) Nothing] ->
    S.Lambda (transformSrcSpan s) pats e
  _ -> error
    (  "Only a single match with an unguarded right-hand side "
    ++ "and no bindings is supported in a lambda abstraction"
    )
transformExpr (GHC.L s (GHC.HsLet _ binds e)) = S.Let
  (transformSrcSpan s)
  (fromMaybe (error "No bindings in let expression") (transformLocalBinds binds)
  )
  (transformExpr e)
transformExpr (GHC.L s (GHC.HsIf _ _ e1 e2 e3)) = S.If (transformSrcSpan s)
                                                       (transformExpr e1)
                                                       (transformExpr e2)
                                                       (transformExpr e3)
transformExpr (GHC.L s (GHC.HsCase _ e mg)) = S.Case
  (transformSrcSpan s)
  (transformExpr e)
  (map matchToAlt (transformMatchGroup mg))
 where
  matchToAlt :: S.Match GHC -> S.Alt GHC
  matchToAlt (S.Match s' _ [pat] rhs mBinds) = S.Alt s' pat rhs mBinds
  matchToAlt _                               = error
    "Only matches with a single pattern are supported in case alternatives"
transformExpr (GHC.L s (GHC.ExplicitTuple _ tArgs boxity)) = S.Tuple
  (transformSrcSpan s)
  (transformBoxity boxity)
  (map transformTupleArg tArgs)
transformExpr (GHC.L s (GHC.ExplicitList _ _ es)) =
  S.List (transformSrcSpan s) (map transformExpr es)
transformExpr (GHC.L s (GHC.HsPar _ e)) =
  S.Paren (transformSrcSpan s) (transformExpr e)
transformExpr (GHC.L s (GHC.ExprWithTySig _ e typeSig)) =
  S.ExpTypeSig (transformSrcSpan s) (transformExpr e) (SigType typeSig)
transformExpr _ = error "Unsupported expression"

-- | Transforms a GHC located tuple argument consisting of an expression into
--   an HST expression.
transformTupleArg :: GHC.LHsTupArg GHC.GhcPs -> S.Exp GHC
transformTupleArg (GHC.L _ (GHC.Present _ e)) = transformExpr e
transformTupleArg _ = error "Missing argument in tuple"

-- | Transforms a GHC located pattern into an HST pattern.
transformPat :: GHC.LPat GHC.GhcPs -> S.Pat GHC
transformPat (GHC.L s (GHC.VarPat _ name)) =
  S.PVar (transformSrcSpan s) (transformRdrNameUnqual name)
transformPat (GHC.L s (GHC.ConPatIn name cpds)) =
  let s'             = transformSrcSpan s
      (name', isCon) = transformRdrName name
  in  case (cpds, isCon) of
        (GHC.InfixCon pat1 pat2, True) ->
          S.PInfixApp s' (transformPat pat1) name' (transformPat pat2)
        (GHC.PrefixCon pats, True) -> S.PApp s' name' (map transformPat pats)
        (_, True) -> error "Record constructors are not supported"
        _ -> error "Only constructors can be applied in patterns"
-- TODO The documentation also mentions a more complicated ConPatOut.
-- Do we need to consider that?
transformPat (GHC.L s (GHC.TuplePat _ pats boxity)) =
  S.PTuple (transformSrcSpan s) (transformBoxity boxity) (map transformPat pats)
transformPat (GHC.L s (GHC.ParPat _ pat)) =
  S.PParen (transformSrcSpan s) (transformPat pat)
transformPat (GHC.L s (GHC.ListPat _ pats)) =
  S.PList (transformSrcSpan s) (map transformPat pats)
transformPat (GHC.L s (GHC.WildPat _)) = S.PWildCard (transformSrcSpan s)
transformPat _                         = error "Unsupported pattern"

-- | Transforms a GHC module name with an HST source span into an HST module
--   name.
transformModuleName :: S.SrcSpan GHC -> GHC.ModuleName -> S.ModuleName GHC
transformModuleName s modName = S.ModuleName s (GHC.moduleNameString modName)

-- | Transforms a GHC located reader name into an HST qualified name and a
--   @Bool@ which is @True@ if the name belongs to a data constructor and
--   @False@ otherwise.
transformRdrName :: GHC.Located GHC.RdrName -> (S.QName GHC, Bool)
transformRdrName (GHC.L s (GHC.Unqual name)) =
  let s' = transformSrcSpan s
  in  (S.UnQual s' (S.Ident s' (GHC.occNameString name)), GHC.isDataOcc name)
transformRdrName (GHC.L s (GHC.Qual modName name)) =
  let s' = transformSrcSpan s
  in  ( S.Qual s'
               (transformModuleName s' modName)
               (S.Ident s' (GHC.occNameString name))
      , GHC.isDataOcc name
      )
transformRdrName (GHC.L s (GHC.Exact name)) =
  let s' = transformSrcSpan s in (S.Special s' (transformName s' name), False)
transformRdrName _ = error "Unsupported RdrName"

-- | Transforms a GHC located unqualified reader name into an HST name.
transformRdrNameUnqual :: GHC.Located GHC.RdrName -> S.Name GHC
transformRdrNameUnqual (GHC.L s (GHC.Unqual occName)) =
  S.Ident (transformSrcSpan s) (GHC.occNameString occName)
transformRdrNameUnqual _ = error "Expected an unqualified name"

-- | Transforms a GHC name with an HST source span into an HST special
--   constructor.
transformName :: S.SrcSpan GHC -> GHC.Name -> S.SpecialCon GHC
transformName s name = case lookup name (specialConMap s) of
  Just sCon -> sCon
  Nothing   -> case GHC.wiredInNameTyThing_maybe name of
    -- TODO Can the other TyThing constructors occur?
    Just (GHC.ATyCon tc) -> case GHC.tyConTuple_maybe tc of
      Just GHC.BoxedTuple   -> S.TupleCon s S.Boxed (GHC.tyConArity tc)
      Just GHC.UnboxedTuple -> S.TupleCon s S.Unboxed (GHC.tyConArity tc)
      _                     -> error "Unexpected special constructor"
    _ -> error "Expected a built-in special constructor"

-- | Takes an HST source span and maps GHC names to HST special constructors.
specialConMap :: S.SrcSpan GHC -> [(GHC.Name, S.SpecialCon GHC)]
specialConMap s =
  [ (GHC.tyConName GHC.unitTyCon          , S.UnitCon s)
  , (GHC.listTyConName                    , S.ListCon s)
  , (GHC.tyConName GHC.funTyCon           , S.FunCon s)
  , (GHC.consDataConName                  , S.Cons s)
  , (GHC.tupleTyConName GHC.UnboxedTuple 1, S.UnboxedSingleCon s)
  ]

-- | Wraps a GHC source span into the HST type for source spans.
transformSrcSpan :: GHC.SrcSpan -> S.SrcSpan GHC
transformSrcSpan = S.SrcSpan
