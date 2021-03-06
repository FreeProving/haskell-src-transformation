-- | This module contains functions transforming Haskell modules and other
--   constructs of the AST data structure of @haskell-src-exts@ into the
--   corresponding constructs of the AST data structure in the
--   "HST.Frontend.Syntax" module.
module HST.Frontend.HSE.From where

import           Data.Maybe                        ( catMaybes )
import qualified Language.Haskell.Exts             as HSE
import           Polysemy                          ( Member, Sem )

import           HST.Effect.Report                 ( Report, reportFatal )
import           HST.Frontend.HSE.Config
  ( HSE, OriginalModuleHead(OriginalModuleHead) )
import qualified HST.Frontend.Syntax               as S
import           HST.Frontend.Transformer.Messages
  ( notSupported, skipNotSupported )
import           HST.Util.Messages                 ( Severity(Error), message )

-------------------------------------------------------------------------------
-- Modules                                                                   --
-------------------------------------------------------------------------------
-- | Transforms the @haskell-src-exts@ representation of a Haskell module into
--   the @haskell-src-transformations@ representation of a Haskell module.
transformModule :: (Member Report r, TransformSrcSpan srcSpan)
                => HSE.Module srcSpan
                -> Sem r (S.Module (HSE srcSpan))
transformModule (HSE.Module s moduleHead pragmas imports decls) = S.Module
  (transformSrcSpan s) (OriginalModuleHead moduleHead pragmas imports)
  <$> mapM transformModuleHead moduleHead
  <*> fmap catMaybes (mapM transformImportDecl imports)
  <*> mapM transformDecl decls
transformModule (HSE.XmlPage s _ _ _ _ _ _)                     = notSupported
  "XML Modules" (transformSrcSpan s)
transformModule (HSE.XmlHybrid s _ _ _ _ _ _ _ _)               = notSupported
  "XML Modules" (transformSrcSpan s)

-- | Extracts the name of a module from a module head.
transformModuleHead :: (Member Report r, TransformSrcSpan srcSpan)
                    => HSE.ModuleHead srcSpan
                    -> Sem r (S.ModuleName (HSE srcSpan))
transformModuleHead (HSE.ModuleHead _ name _ _) = transformModuleName name

-------------------------------------------------------------------------------
-- Declarations                                                              --
-------------------------------------------------------------------------------
-- | Transforms an HSE declaration into an HST declaration.
--
--   Unsupported declarations are preserved by wrapping them in the
--   'S.OtherDecl' constructor.
transformDecl :: (Member Report r, TransformSrcSpan srcSpan)
              => HSE.Decl srcSpan -- ^ The declaration to transform.
              -> Sem r (S.Decl (HSE srcSpan))

-- Data type and newtype declarations are supported.
transformDecl decl@(HSE.DataDecl s _ _ dHead qcds _) = do
  dHead' <- transformDeclHead dHead
  qcds' <- mapM transformQualConDecl qcds
  return $ S.DataDecl (transformSrcSpan s) decl dHead' qcds'
-- Function declarations are supported.
transformDecl (HSE.FunBind s matches) = do
  matches' <- mapM transformMatch matches
  return $ S.FunBind (transformSrcSpan s) matches'
-- Only variable pattern bindings are supported. The user is informed if there
-- are unsupported variable patterns that are skipped.
transformDecl (HSE.PatBind s (HSE.PVar _ name) rhs mBinds) = do
  match' <- transformMatch (HSE.Match s name [] rhs mBinds)
  return $ S.FunBind (transformSrcSpan s) [match']
transformDecl decl@(HSE.PatBind s _ _ _) = do
  skipNotSupported "Non-variable pattern bindings" (transformSrcSpan s)
  return $ S.OtherDecl (transformSrcSpan s) decl
-- Type classes and type class instances are not supported. The user is
-- explicitly informed that the declaration is skipped since they might
-- contain pattern matching.
transformDecl decl@(HSE.ClassDecl s _ _ _ _) = do
  skipNotSupported "Type classes" (transformSrcSpan s)
  return $ S.OtherDecl (transformSrcSpan s) decl
transformDecl decl@(HSE.InstDecl s _ _ _) = do
  skipNotSupported "Type class instances" (transformSrcSpan s)
  return $ S.OtherDecl (transformSrcSpan s) decl
-- GADTs and pattern synonyms are not supported. The user is explicitly
-- informed that the declaration is skipped since there may be errors due
-- to the skipped constructor or pattern declarations.
transformDecl decl@(HSE.GDataDecl s _ _ _ _ _ _) = do
  skipNotSupported "GADTs" (transformSrcSpan s)
  return $ S.OtherDecl (transformSrcSpan s) decl
transformDecl decl@(HSE.PatSyn s _ _ _) = do
  skipNotSupported "Pattern synonyms" (transformSrcSpan s)
  return $ S.OtherDecl (transformSrcSpan s) decl
-- Type and data families are not supported. The user is informed of skipped
-- data instances only since all type family declarations and instances as well
-- as data family declarations don't contain constructors.
transformDecl decl@(HSE.TypeFamDecl s _ _ _) = do
  return $ S.OtherDecl (transformSrcSpan s) decl
transformDecl decl@(HSE.ClosedTypeFamDecl s _ _ _ _) = do
  return $ S.OtherDecl (transformSrcSpan s) decl
transformDecl decl@(HSE.DataFamDecl s _ _ _) = do
  return $ S.OtherDecl (transformSrcSpan s) decl
transformDecl decl@(HSE.TypeInsDecl s _ _) = do
  return $ S.OtherDecl (transformSrcSpan s) decl
transformDecl decl@(HSE.DataInsDecl s _ _ _ _) = do
  skipNotSupported "Data family instances" (transformSrcSpan s)
  return $ S.OtherDecl (transformSrcSpan s) decl
transformDecl decl@(HSE.GDataInsDecl s _ _ _ _ _) = do
  skipNotSupported "GADT-style data family instances" (transformSrcSpan s)
  return $ S.OtherDecl (transformSrcSpan s) decl
-- Template Haskell is not supported. The user is informed when
-- splices are skipped since they contain expressions that are
-- not transformed.
transformDecl decl@(HSE.SpliceDecl s _) = do
  skipNotSupported "Template Haskell splicing declarations" (transformSrcSpan s)
  return $ S.OtherDecl (transformSrcSpan s) decl
transformDecl decl@(HSE.TSpliceDecl s _) = do
  skipNotSupported "Template Haskell splicing declarations" (transformSrcSpan s)
  return $ S.OtherDecl (transformSrcSpan s) decl
-- Type signatures, fixity declarations and pragmas are skipped silently.
transformDecl decl@(HSE.TypeSig s _ _) = return
  $ S.OtherDecl (transformSrcSpan s) decl
transformDecl decl@(HSE.PatSynSig s _ _ _ _ _ _) = do
  return $ S.OtherDecl (transformSrcSpan s) decl
transformDecl decl@(HSE.InfixDecl s _ _ _) = return
  $ S.OtherDecl (transformSrcSpan s) decl
transformDecl decl@(HSE.RulePragmaDecl s _) = return
  $ S.OtherDecl (transformSrcSpan s) decl
transformDecl decl@(HSE.DeprPragmaDecl s _) = return
  $ S.OtherDecl (transformSrcSpan s) decl
transformDecl decl@(HSE.WarnPragmaDecl s _) = return
  $ S.OtherDecl (transformSrcSpan s) decl
transformDecl decl@(HSE.AnnPragma s _) = return
  $ S.OtherDecl (transformSrcSpan s) decl
transformDecl decl@(HSE.MinimalPragma s _) = return
  $ S.OtherDecl (transformSrcSpan s) decl
transformDecl decl@(HSE.RoleAnnotDecl s _ _) = return
  $ S.OtherDecl (transformSrcSpan s) decl
transformDecl decl@(HSE.CompletePragma s _ _) = return
  $ S.OtherDecl (transformSrcSpan s) decl
transformDecl decl@(HSE.InlineSig s _ _ _) = return
  $ S.OtherDecl (transformSrcSpan s) decl
transformDecl decl@(HSE.InlineConlikeSig s _ _) = return
  $ S.OtherDecl (transformSrcSpan s) decl
transformDecl decl@(HSE.SpecSig s _ _ _) = return
  $ S.OtherDecl (transformSrcSpan s) decl
transformDecl decl@(HSE.SpecInlineSig s _ _ _ _) = return
  $ S.OtherDecl (transformSrcSpan s) decl
transformDecl decl@(HSE.InstSig s _) = return
  $ S.OtherDecl (transformSrcSpan s) decl
-- All other declarations are not supported and preserved unchanged without
-- explicitly informing the user.
transformDecl decl@(HSE.TypeDecl s _ _) = return
  $ S.OtherDecl (transformSrcSpan s) decl
transformDecl decl@(HSE.DerivDecl s _ _ _) = return
  $ S.OtherDecl (transformSrcSpan s) decl
transformDecl decl@(HSE.DefaultDecl s _) = return
  $ S.OtherDecl (transformSrcSpan s) decl
transformDecl decl@(HSE.ForImp s _ _ _ _ _) = return
  $ S.OtherDecl (transformSrcSpan s) decl
transformDecl decl@(HSE.ForExp s _ _ _ _) = return
  $ S.OtherDecl (transformSrcSpan s) decl

-- | Transforms an HSE import declaration into an HST import declaration.
--
--   Unsupported import declarations are skipped and the user is informed
--   about them as missing imports could lead to further errors.
transformImportDecl :: (Member Report r, TransformSrcSpan srcSpan)
                    => HSE.ImportDecl srcSpan
                    -> Sem r (Maybe (S.ImportDecl (HSE srcSpan)))
transformImportDecl HSE.ImportDecl { HSE.importSrc = True, HSE.importAnn = s }
  = skipNotSupported "{-# SOURCE #-} imports" (transformSrcSpan s)
  >> return Nothing
transformImportDecl HSE.ImportDecl { HSE.importSafe = True, HSE.importAnn = s }
  = skipNotSupported "Safe imports" (transformSrcSpan s) >> return Nothing
transformImportDecl
  HSE.ImportDecl { HSE.importPkg = Just _, HSE.importAnn = s }
  = skipNotSupported "Package imports" (transformSrcSpan s) >> return Nothing
transformImportDecl
  HSE.ImportDecl { HSE.importSpecs = Just _, HSE.importAnn = s }
  = skipNotSupported "Import specifications" (transformSrcSpan s)
  >> return Nothing
transformImportDecl importDecl = do
  importModule <- transformModuleName (HSE.importModule importDecl)
  aliasName <- traverse transformModuleName (HSE.importAs importDecl)
  return
    $ Just S.ImportDecl
    { S.importSrcSpan = transformSrcSpan (HSE.importAnn importDecl)
    , S.importModule  = importModule
    , S.importIsQual  = HSE.importQualified importDecl
    , S.importAsName  = aliasName
    }

-------------------------------------------------------------------------------
-- Data Type Declarations                                                    --
-------------------------------------------------------------------------------
-- | Transforms an HSE declaration head into an HST declaration head.
transformDeclHead :: TransformSrcSpan srcSpan
                  => HSE.DeclHead srcSpan
                  -> Sem r (S.Name (HSE srcSpan))
transformDeclHead (HSE.DHead _ dName)     = transformName dName
transformDeclHead (HSE.DHInfix _ _ dName) = transformName dName
transformDeclHead (HSE.DHParen _ dHead)   = transformDeclHead dHead
transformDeclHead (HSE.DHApp _ dHead _)   = transformDeclHead dHead

-- | Transforms an HSE qualified constructor declaration into an HST
--   constructor declaration.
transformQualConDecl :: (Member Report r, TransformSrcSpan srcSpan)
                     => HSE.QualConDecl srcSpan
                     -> Sem r (S.ConDecl (HSE srcSpan))
transformQualConDecl (HSE.QualConDecl _ _ _ conDecl) = transformConDecl conDecl

-- | Transforms an HSE constructor declaration into an HST constructor
--   declaration.
transformConDecl :: (Member Report r, TransformSrcSpan srcSpan)
                 => HSE.ConDecl srcSpan
                 -> Sem r (S.ConDecl (HSE srcSpan))
transformConDecl (HSE.ConDecl s cName types)    = do
  name' <- transformName cName
  return S.ConDecl { S.conDeclSrcSpan = transformSrcSpan s
                   , S.conDeclName    = name'
                   , S.conDeclArity   = length types
                   , S.conDeclIsInfix = False
                   }
transformConDecl (HSE.InfixConDecl s _ cName _) = do
  name' <- transformName cName
  return S.ConDecl { S.conDeclSrcSpan = transformSrcSpan s
                   , S.conDeclName    = name'
                   , S.conDeclArity   = 2
                   , S.conDeclIsInfix = True
                   }
transformConDecl (HSE.RecDecl s _ _)            = notSupported "Records"
  (transformSrcSpan s)

-------------------------------------------------------------------------------
-- Function Declarations                                                     --
-------------------------------------------------------------------------------
-- | Transforms an HSE binding group into an HST binding group.
transformBinds :: (Member Report r, TransformSrcSpan srcSpan)
               => HSE.Binds srcSpan
               -> Sem r (S.Binds (HSE srcSpan))
transformBinds (HSE.BDecls s decls) = S.BDecls (transformSrcSpan s)
  <$> mapM transformDecl decls
transformBinds (HSE.IPBinds s _)    = notSupported "Implicit-parameters"
  (transformSrcSpan s)

-- | Transforms an HSE match into an HST match.
transformMatch :: (Member Report r, TransformSrcSpan srcSpan)
               => HSE.Match srcSpan
               -> Sem r (S.Match (HSE srcSpan))
transformMatch (HSE.Match s name pats rhs mBinds)
  = S.Match (transformSrcSpan s) False <$> transformName name
  <*> mapM transformPat pats
  <*> transformRhs rhs
  <*> mapM transformBinds mBinds
transformMatch (HSE.InfixMatch s pat name pats rhs mBinds)
  = S.Match (transformSrcSpan s) True <$> transformName name
  <*> mapM transformPat (pat : pats)
  <*> transformRhs rhs
  <*> mapM transformBinds mBinds

-- | Transforms an HSE right hand side into an HST right hand side.
transformRhs :: (Member Report r, TransformSrcSpan srcSpan)
             => HSE.Rhs srcSpan
             -> Sem r (S.Rhs (HSE srcSpan))
transformRhs (HSE.UnGuardedRhs s e)    = S.UnGuardedRhs (transformSrcSpan s)
  <$> transformExp e
transformRhs (HSE.GuardedRhss s grhss) = S.GuardedRhss (transformSrcSpan s)
  <$> mapM transformGuardedRhs grhss

-- | Transforms an HSE guarded right hand side into an HST guarded right hand
--   side.
transformGuardedRhs :: (Member Report r, TransformSrcSpan srcSpan)
                    => HSE.GuardedRhs srcSpan
                    -> Sem r (S.GuardedRhs (HSE srcSpan))
transformGuardedRhs (HSE.GuardedRhs s [HSE.Qualifier _ ge] e)
  = S.GuardedRhs (transformSrcSpan s) <$> transformExp ge <*> transformExp e
transformGuardedRhs (HSE.GuardedRhs s _ _)                    = notSupported
  "Pattern guards" (transformSrcSpan s)

-------------------------------------------------------------------------------
-- Expressions                                                               --
-------------------------------------------------------------------------------
-- | Transforms an HSE boxed mark into an HST boxed mark.
transformBoxed :: HSE.Boxed -> Sem r S.Boxed
transformBoxed HSE.Boxed   = return S.Boxed
transformBoxed HSE.Unboxed = return S.Unboxed

-- | Transforms an HSE expression into an HST expression.
transformExp :: (Member Report r, TransformSrcSpan srcSpan)
             => HSE.Exp srcSpan
             -> Sem r (S.Exp (HSE srcSpan))
transformExp (HSE.Var s qName)                = S.Var (transformSrcSpan s)
  <$> transformQName qName
transformExp (HSE.Con s qName)                = S.Con (transformSrcSpan s)
  <$> transformQName qName
transformExp (HSE.Lit s lit)                  = return
  (S.Lit (transformSrcSpan s) lit)
transformExp (HSE.InfixApp s e1 qOp e2)       = S.InfixApp (transformSrcSpan s)
  <$> transformExp e1
  <*> transformQOp qOp
  <*> transformExp e2
transformExp (HSE.App s e1 e2)
  = S.App (transformSrcSpan s) <$> transformExp e1 <*> transformExp e2
transformExp (HSE.NegApp s e)                 = S.NegApp (transformSrcSpan s)
  <$> transformExp e
transformExp (HSE.Lambda s pats e)
  = S.Lambda (transformSrcSpan s) <$> mapM transformPat pats <*> transformExp e
transformExp (HSE.Let s binds e)
  = S.Let (transformSrcSpan s) <$> transformBinds binds <*> transformExp e
transformExp (HSE.If s e1 e2 e3)              = S.If (transformSrcSpan s)
  <$> transformExp e1
  <*> transformExp e2
  <*> transformExp e3
transformExp (HSE.Case s e alts)
  = S.Case (transformSrcSpan s) <$> transformExp e <*> mapM transformAlt alts
transformExp (HSE.Tuple s bxd es)             = S.Tuple (transformSrcSpan s)
  <$> transformBoxed bxd
  <*> mapM transformExp es
transformExp (HSE.List s es)                  = S.List (transformSrcSpan s)
  <$> mapM transformExp es
transformExp (HSE.Paren s e)                  = S.Paren (transformSrcSpan s)
  <$> transformExp e
transformExp (HSE.ExpTypeSig s e typ)
  = S.ExpTypeSig (transformSrcSpan s) <$> transformExp e <*> return typ
-- All other expressions are not supported.
transformExp (HSE.OverloadedLabel s _)        = notSupported "Overloaded labels"
  (transformSrcSpan s)
transformExp (HSE.IPVar s _)                  = notSupported
  "Implicit-parameters" (transformSrcSpan s)
transformExp (HSE.MultiIf s _)                = notSupported
  "Multi-Way if-expressions" (transformSrcSpan s)
transformExp (HSE.Do s _)                     = notSupported "do-expressions"
  (transformSrcSpan s)
transformExp (HSE.MDo s _)                    = notSupported "mdo-expressions"
  (transformSrcSpan s)
transformExp (HSE.UnboxedSum s _ _ _)         = notSupported "Unboxed sums"
  (transformSrcSpan s)
transformExp (HSE.TupleSection s _ _)         = notSupported "Tuple sections"
  (transformSrcSpan s)
transformExp (HSE.ParArray s _)               = notSupported "Parallel arrays"
  (transformSrcSpan s)
transformExp (HSE.LeftSection s _ _)          = notSupported "Sections"
  (transformSrcSpan s)
transformExp (HSE.RightSection s _ _)         = notSupported "Sections"
  (transformSrcSpan s)
transformExp (HSE.RecConstr s _ _)            = notSupported "Records"
  (transformSrcSpan s)
transformExp (HSE.RecUpdate s _ _)            = notSupported "Records"
  (transformSrcSpan s)
transformExp (HSE.EnumFrom s _)               = notSupported "Enumerations"
  (transformSrcSpan s)
transformExp (HSE.EnumFromTo s _ _)           = notSupported "Enumerations"
  (transformSrcSpan s)
transformExp (HSE.EnumFromThen s _ _)         = notSupported "Enumerations"
  (transformSrcSpan s)
transformExp (HSE.EnumFromThenTo s _ _ _)     = notSupported "Enumerations"
  (transformSrcSpan s)
transformExp (HSE.ParArrayFromTo s _ _)       = notSupported "Parallel arrays"
  (transformSrcSpan s)
transformExp (HSE.ParArrayFromThenTo s _ _ _) = notSupported "Parallel arrays"
  (transformSrcSpan s)
transformExp (HSE.ListComp s _ _)             = notSupported
  "List comprehensions" (transformSrcSpan s)
transformExp (HSE.ParComp s _ _)              = notSupported
  "List comprehensions" (transformSrcSpan s)
transformExp (HSE.ParArrayComp s _ _)         = notSupported "Parallel arrays"
  (transformSrcSpan s)
transformExp (HSE.VarQuote s _)               = notSupported
  "Template Haskell expressions" (transformSrcSpan s)
transformExp (HSE.TypQuote s _)               = notSupported
  "Template Haskell expressions" (transformSrcSpan s)
transformExp (HSE.BracketExp s _)             = notSupported
  "Template Haskell expressions" (transformSrcSpan s)
transformExp (HSE.SpliceExp s _)              = notSupported
  "Template Haskell expressions" (transformSrcSpan s)
transformExp (HSE.QuasiQuote s _ _)           = notSupported
  "Template Haskell expressions" (transformSrcSpan s)
transformExp (HSE.TypeApp s _)                = notSupported
  "Visible type applications" (transformSrcSpan s)
transformExp (HSE.XTag s _ _ _ _)             = notSupported "XML expressions"
  (transformSrcSpan s)
transformExp (HSE.XETag s _ _ _)              = notSupported "XML expressions"
  (transformSrcSpan s)
transformExp (HSE.XPcdata s _)                = notSupported "XML expressions"
  (transformSrcSpan s)
transformExp (HSE.XExpTag s _)                = notSupported "XML expressions"
  (transformSrcSpan s)
transformExp (HSE.XChildTag s _)              = notSupported "XML expressions"
  (transformSrcSpan s)
transformExp (HSE.CorePragma s _ _)           = notSupported "CORE pragmas"
  (transformSrcSpan s)
transformExp (HSE.SCCPragma s _ _)            = notSupported "SCC pragmas"
  (transformSrcSpan s)
transformExp (HSE.GenPragma s _ _ _ _)        = notSupported "GENERATED pragmas"
  (transformSrcSpan s)
transformExp (HSE.Proc s _ _)                 = notSupported "Arrow expressions"
  (transformSrcSpan s)
transformExp (HSE.LeftArrApp s _ _)           = notSupported "Arrow expressions"
  (transformSrcSpan s)
transformExp (HSE.RightArrApp s _ _)          = notSupported "Arrow expressions"
  (transformSrcSpan s)
transformExp (HSE.LeftArrHighApp s _ _)       = notSupported "Arrow expressions"
  (transformSrcSpan s)
transformExp (HSE.RightArrHighApp s _ _)      = notSupported "Arrow expressions"
  (transformSrcSpan s)
transformExp (HSE.ArrOp s _)                  = notSupported "Arrow expressions"
  (transformSrcSpan s)
transformExp (HSE.LCase s _)                  = notSupported
  "Lambda case expressions" (transformSrcSpan s)

-- | Transforms an HSE case alternative into an HST case alternative.
transformAlt :: (Member Report r, TransformSrcSpan srcSpan)
             => HSE.Alt srcSpan
             -> Sem r (S.Alt (HSE srcSpan))
transformAlt (HSE.Alt s pat rhs mBinds) = S.Alt (transformSrcSpan s)
  <$> transformPat pat
  <*> transformRhs rhs
  <*> mapM transformBinds mBinds

-------------------------------------------------------------------------------
-- Patterns                                                                  --
-------------------------------------------------------------------------------
-- | Transforms an HSE pattern into an HST pattern.
transformPat :: (Member Report r, TransformSrcSpan srcSpan)
             => HSE.Pat srcSpan
             -> Sem r (S.Pat (HSE srcSpan))
transformPat (HSE.PVar s name)                 = S.PVar (transformSrcSpan s)
  <$> transformName name
transformPat (HSE.PInfixApp s pat1 qName pat2)
  = S.PInfixApp (transformSrcSpan s) <$> transformPat pat1
  <*> transformQName qName
  <*> transformPat pat2
transformPat (HSE.PApp s qName pats)           = S.PApp (transformSrcSpan s)
  <$> transformQName qName
  <*> mapM transformPat pats
transformPat (HSE.PTuple s bxd pats)           = S.PTuple (transformSrcSpan s)
  <$> transformBoxed bxd
  <*> mapM transformPat pats
transformPat (HSE.PParen s pat)                = S.PParen (transformSrcSpan s)
  <$> transformPat pat
transformPat (HSE.PList s pats)                = S.PList (transformSrcSpan s)
  <$> mapM transformPat pats
transformPat (HSE.PWildCard s)                 = return
  (S.PWildCard (transformSrcSpan s))
-- All other patterns are not supported.
transformPat (HSE.PLit s _ _)                  = notSupported "Literal patterns"
  (transformSrcSpan s)
transformPat (HSE.PNPlusK s _ _)               = notSupported "n+k patterns"
  (transformSrcSpan s)
transformPat (HSE.PUnboxedSum s _ _ _)         = notSupported "Unboxed sums"
  (transformSrcSpan s)
transformPat (HSE.PRec s _ _)                  = notSupported "Records"
  (transformSrcSpan s)
transformPat (HSE.PAsPat s _ _)                = notSupported "as-patterns"
  (transformSrcSpan s)
transformPat (HSE.PIrrPat s _)                 = notSupported
  "Irrefutable patterns" (transformSrcSpan s)
transformPat (HSE.PatTypeSig s _ _)            = notSupported
  "Patterns with type signatures" (transformSrcSpan s)
transformPat (HSE.PViewPat s _ _)              = notSupported "View patterns"
  (transformSrcSpan s)
transformPat (HSE.PRPat s _)                   = notSupported "Regular patterns"
  (transformSrcSpan s)
transformPat (HSE.PXTag s _ _ _ _)             = notSupported "XML patterns"
  (transformSrcSpan s)
transformPat (HSE.PXETag s _ _ _)              = notSupported "XML patterns"
  (transformSrcSpan s)
transformPat (HSE.PXPcdata s _)                = notSupported "XML patterns"
  (transformSrcSpan s)
transformPat (HSE.PXPatTag s _)                = notSupported "XML patterns"
  (transformSrcSpan s)
transformPat (HSE.PXRPats s _)                 = notSupported "XML patterns"
  (transformSrcSpan s)
transformPat (HSE.PSplice s _)                 = notSupported "Template Haskell"
  (transformSrcSpan s)
transformPat (HSE.PQuasiQuote s _ _)           = notSupported "Template Haskell"
  (transformSrcSpan s)
transformPat (HSE.PBangPat s _)                = notSupported "Bang patterns"
  (transformSrcSpan s)

-------------------------------------------------------------------------------
-- Names                                                                     --
-------------------------------------------------------------------------------
-- | Transforms an HSE module name into an HST module name.
transformModuleName :: TransformSrcSpan srcSpan
                    => HSE.ModuleName srcSpan
                    -> Sem r (S.ModuleName (HSE srcSpan))
transformModuleName (HSE.ModuleName s name) = return
  $ S.ModuleName (transformSrcSpan s) name

-- | Transforms an HSE qualified name into an HST qualified name.
transformQName :: (Member Report r, TransformSrcSpan srcSpan)
               => HSE.QName srcSpan
               -> Sem r (S.QName (HSE srcSpan))
transformQName (HSE.Qual s modName name) = S.Qual (transformSrcSpan s)
  <$> transformModuleName modName
  <*> transformName name
transformQName (HSE.UnQual s name)       = S.UnQual (transformSrcSpan s)
  <$> transformName name
transformQName (HSE.Special s spCon)     = S.Special (transformSrcSpan s)
  <$> transformSpecialCon spCon

-- | Transforms an HSE name into an HST name.
transformName :: TransformSrcSpan srcSpan
              => HSE.Name srcSpan
              -> Sem r (S.Name (HSE srcSpan))
transformName (HSE.Ident s name)  = return $ S.Ident (transformSrcSpan s) name
transformName (HSE.Symbol s name) = return $ S.Symbol (transformSrcSpan s) name

-- | Transforms an HSE qualified operator into an HST qualified operator.
transformQOp :: (Member Report r, TransformSrcSpan srcSpan)
             => HSE.QOp srcSpan
             -> Sem r (S.QOp (HSE srcSpan))
transformQOp (HSE.QVarOp s qName) = S.QVarOp (transformSrcSpan s)
  <$> transformQName qName
transformQOp (HSE.QConOp s qName) = S.QConOp (transformSrcSpan s)
  <$> transformQName qName

-- | Transforms an HSE special constructor into an HST special constructor.
transformSpecialCon :: (Member Report r, TransformSrcSpan srcSpan)
                    => HSE.SpecialCon srcSpan
                    -> Sem r (S.SpecialCon (HSE srcSpan))
transformSpecialCon (HSE.UnitCon s)          = return
  $ S.UnitCon (transformSrcSpan s)
transformSpecialCon (HSE.UnboxedSingleCon s) = return
  $ S.UnboxedSingleCon (transformSrcSpan s)
transformSpecialCon (HSE.TupleCon s bxd n)
  = S.TupleCon (transformSrcSpan s) <$> transformBoxed bxd <*> return n
transformSpecialCon (HSE.ListCon s)          = return
  $ S.NilCon (transformSrcSpan s)
transformSpecialCon (HSE.Cons s)             = return
  $ S.ConsCon (transformSrcSpan s)
transformSpecialCon (HSE.ExprHole s)         = return
  $ S.ExprHole (transformSrcSpan s)
transformSpecialCon (HSE.FunCon s)           = reportFatal
  $ message Error (transformSrcSpan s) "Expected data constructor but got (->)."

-------------------------------------------------------------------------------
-- Source Spans                                                              --
-------------------------------------------------------------------------------
-- | Type class for annotation types that can be used as a source span of
--   the @haskell-src-exts@ AST.
class TransformSrcSpan srcSpan where
  transformSrcSpan :: srcSpan -> S.SrcSpan (HSE srcSpan)

-- | Wraps an HSE source span into the HST type for source spans.
instance TransformSrcSpan HSE.SrcSpanInfo where
  transformSrcSpan srcSpanInfo
    | HSE.isNullSpan srcSpan = S.NoSrcSpan
    | otherwise = S.SrcSpan srcSpanInfo S.MsgSrcSpan
      { S.msgSrcSpanFilePath    = HSE.srcSpanFilename srcSpan
      , S.msgSrcSpanStartLine   = HSE.srcSpanStartLine srcSpan
      , S.msgSrcSpanStartColumn = HSE.srcSpanStartColumn srcSpan
      , S.msgSrcSpanEndLine     = HSE.srcSpanEndLine srcSpan
      , S.msgSrcSpanEndColumn   = HSE.srcSpanEndColumn srcSpan
      }
   where
    srcSpan :: HSE.SrcSpan
    srcSpan = HSE.srcInfoSpan srcSpanInfo
