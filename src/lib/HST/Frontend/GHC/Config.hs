{-# LANGUAGE TypeFamilies #-}

-- | This module configures the HST syntax for the usage of @ghc-lib-parser@ as
--   the front end by instantiating the type families of 'HST.Frontend.Syntax'
--   with concrete types of the @ghc-lib-parser@ AST data structure or wrappers
--   for these types.
--
--   This module also defines all instances necessary for being able to use
--   @Eq@ and @Show@ for every component of the GHC-instantiated HST syntax.
module HST.Frontend.GHC.Config where

import qualified BasicTypes                                          as GHC
import           Data.Data                                           ( Data )
import           Data.List
  ( intercalate )
import qualified DynFlags                                            as GHC
import qualified GHC.Hs                                              as GHC
import qualified GHC.Hs.Dump                                         as GHC
import qualified Language.Haskell.GhclibParserEx.GHC.Settings.Config as GHC
import qualified Module                                              as GHC
import qualified Outputable                                          as GHC
import qualified SrcLoc                                              as GHC

import qualified HST.Frontend.Syntax                                 as S

-------------------------------------------------------------------------------
-- Type Family Instances                                                     --
-------------------------------------------------------------------------------
-- | Type representing the AST data structure of @ghc-lib-parser@.
--
--   Instantiates the type families for source spans, literals, type
--   expressions, additional data from the original module and original
--   declarations with the concrete types from @ghc-lib-parser@ or wrappers for
--   these types. Also adds instances for 'S.EqAST', 'S.ShowAST' and
--   'S.SimpleLoc' to allow the usage of @==@ and @show@ for all AST components
--   and 'S.toSimpleSourceSpan' for source spans.
data GHC

type instance S.SrcSpanType GHC = SrcWrapper

type instance S.Literal GHC = LitWrapper

type instance S.TypeExp GHC = TypeWrapper

type instance S.OriginalModuleHead GHC = OriginalModuleHead

type instance S.OriginalDecl GHC = DeclWrapper

instance S.EqAST GHC

instance S.ShowAST GHC

instance S.SimpleLoc GHC

-------------------------------------------------------------------------------
-- Wrappers for @ghc-lib-parser@ Types                                       --
-------------------------------------------------------------------------------
-- | Wrapper for the two literal types (for regular and overloaded literals)
--   used by @ghc-lib-parser@.
data LitWrapper = Lit (GHC.HsLit GHC.GhcPs) | OverLit (GHC.HsOverLit GHC.GhcPs)
 deriving Eq

instance Show LitWrapper where
  show (Lit l)     = defaultPrintShow l
  show (OverLit l) = defaultPrintShow l

-- | Wrapper for the type for type expressions appearing in type signatures
--   used by @ghc-lib-parser@.
newtype TypeWrapper = SigType (GHC.LHsSigWcType GHC.GhcPs)

instance Eq TypeWrapper where
  SigType t1 == SigType t2 = defaultPrintEq t1 == defaultPrintEq t2

instance Show TypeWrapper where
  show (SigType t) = defaultPrintShow t

-- | Wrapper for the fields of modules that are not supported.
data OriginalModuleHead = OriginalModuleHead
  { originalModuleName             :: Maybe (GHC.Located GHC.ModuleName)
  , originalModuleExports          :: Maybe (GHC.Located [GHC.LIE GHC.GhcPs])
  , originalModuleImports          :: [GHC.LImportDecl GHC.GhcPs]
  , originalModuleDeprecMessage    :: Maybe (GHC.Located GHC.WarningTxt)
  , originalModuleHaddockModHeader :: Maybe GHC.LHsDocString
  }

-- Is there a better solution for these instances? Deriving Eq or Show would be
-- possible if this instance was available for every sub type of the record,
-- but that would require Orphan instances or otherwise unnecessary wrappers
-- for all of these sub types.
instance Eq OriginalModuleHead where
  omh1 == omh2 = and
    [ originalModuleName omh1 == originalModuleName omh2
    , originalModuleExports omh1 == originalModuleExports omh2
    , defaultPrintEq (originalModuleImports omh1)
        == defaultPrintEq (originalModuleImports omh2)
    , originalModuleDeprecMessage omh1 == originalModuleDeprecMessage omh2
    , originalModuleHaddockModHeader omh1
        == originalModuleHaddockModHeader omh2
    ]

instance Show OriginalModuleHead where
  show omh = "OriginalModuleHead {"
    ++ intercalate ", "
    [ "originalModuleName = " ++ defaultPrintShow (originalModuleName omh)
    , "originalModuleExports = "
        ++ defaultPrintShow (originalModuleExports omh)
    , "originalModuleImports = "
        ++ defaultPrintShow (originalModuleImports omh)
    , "originalModuleDeprecMessage = "
        ++ defaultPrintShow (originalModuleDeprecMessage omh)
    , "originalModuleHaddockModHeader = "
        ++ defaultPrintShow (originalModuleHaddockModHeader omh)
    ]
    ++ "}"

-- | Wrapper for the declaration type used by @ghc-lib-parser@.
--
--   Used to store declarations of the original module that are not supported
--   by the pattern matching compiler.
newtype DeclWrapper = Decl (GHC.LHsDecl GHC.GhcPs)

instance Eq DeclWrapper where
  Decl d1 == Decl d2 = defaultPrintEq d1 == defaultPrintEq d2

instance Show DeclWrapper where
  show (Decl d) = defaultPrintShow d

-- | Wrapper for the source span type used by @ghc-lib-parser@.
newtype SrcWrapper = SrcWrapper GHC.SrcSpan
 deriving ( Eq, Show )

instance S.ToSimpleSrcSpan SrcWrapper where
  toSimpleSrcSpan (SrcWrapper (GHC.RealSrcSpan realSrcSpan)) = Just
    S.SimpleSrcSpan { S.startLine   = GHC.srcSpanStartLine realSrcSpan
                    , S.startColumn = GHC.srcSpanStartCol realSrcSpan
                    , S.endLine     = GHC.srcSpanEndLine realSrcSpan
                    , S.endColumn   = GHC.srcSpanEndCol realSrcSpan
                    }
  toSimpleSrcSpan (SrcWrapper (GHC.UnhelpfulSpan _))         = Nothing

-------------------------------------------------------------------------------
-- Printing Functions for the @ghc-lib-parser@ AST                           --
-------------------------------------------------------------------------------
-- | Prints a component of the @ghc-lib-parser@ AST data structure without
--   source span information and with line breaks.
--
--   This function is used for equality of @ghc-lib-parser@ AST components.
defaultPrintEq :: Data a => a -> String
defaultPrintEq d = GHC.showSDoc defaultDynFlags
  (GHC.showAstData GHC.BlankSrcSpan d)

-- | Prints a component of the @ghc-lib-parser@ AST data structure with source
--   span information and without line breaks.
--
--   This function is used for showing @ghc-lib-parser@ AST components.
defaultPrintShow :: Data a => a -> String
defaultPrintShow d = GHC.showSDocOneLine defaultDynFlags
  (GHC.showAstData GHC.NoBlankSrcSpan d)

-- | Returns default variants of the dynamic flags used by @ghc-lib-parser@.
--
--   These dynamic flags are used for parsing Haskell code and printing AST
--   components.
defaultDynFlags :: GHC.DynFlags

-- TODO These DynFlags should be the same as the ones used for parsing Haskell
-- modules and should be defined in another module.
defaultDynFlags = GHC.defaultDynFlags GHC.fakeSettings GHC.fakeLlvmConfig
