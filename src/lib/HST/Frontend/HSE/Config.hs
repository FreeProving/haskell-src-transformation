{-# LANGUAGE TypeFamilies #-}

-- | This module configures the HST syntax for the usage of @haskell-src-exts@
--   as the front end by instantiating the type families of
--   'HST.Frontend.Syntax' with concrete types of the @haskell-src-exts@ AST
--   data structure or wrappers for these types.
--
--   Because of the existing instances of the @haskell-src-exts@ AST, @Eq@ and
--   @Show@ can be used for every component of the HSE-instantiated HST syntax.
module HST.Frontend.HSE.Config where

import qualified Language.Haskell.Exts as HSE

import qualified HST.Frontend.Syntax   as S

-------------------------------------------------------------------------------
-- Type Family Instances                                                     --
-------------------------------------------------------------------------------
-- | Type representing the AST data structure of @haskell-src-exts@.
--
--   Instantiates the type families for source spans, literals, type
--   expressions, additional data from the original module and original
--   declarations with the concrete types from @haskell-src-exts@ or wrappers
--   for these types. Also adds instances for 'S.EqAST', 'S.ShowAST' and
--   'S.SimpleLoc' to allow the usage of @==@ and @show@ for all AST components
--   and 'S.toSimpleSourceSpan' for source spans.
data HSE

type instance S.SrcSpanType HSE = SrcWrapper

type instance S.Literal HSE = HSE.Literal HSE.SrcSpanInfo

type instance S.TypeExp HSE = HSE.Type HSE.SrcSpanInfo

type instance S.OriginalModuleHead HSE = OriginalModuleHead

type instance S.OriginalDecl HSE = HSE.Decl HSE.SrcSpanInfo

instance S.EqAST HSE

instance S.ShowAST HSE

instance S.SimpleLoc HSE

-------------------------------------------------------------------------------
-- Wrappers for @haskell-src-exts@ Types                                     --
-------------------------------------------------------------------------------
-- | Wrapper for the fields of modules that are not supported.
data OriginalModuleHead = OriginalModuleHead
  { originalModuleHead    :: Maybe (HSE.ModuleHead HSE.SrcSpanInfo)
  , originalModulePragmas :: [HSE.ModulePragma HSE.SrcSpanInfo]
  , originalModuleImports :: [HSE.ImportDecl HSE.SrcSpanInfo]
  }
 deriving ( Eq, Show )

-- | Wrapper for the source span type used by @haskell-src-exts@.
newtype SrcWrapper = SrcWrapper HSE.SrcSpanInfo
 deriving ( Eq, Show )

instance S.ToSimpleSrcSpan SrcWrapper where
  toSimpleSrcSpan (SrcWrapper srcSpanInfo)
    = let srcSpan = HSE.srcInfoSpan srcSpanInfo
      in if HSE.isNullSpan srcSpan
           then Nothing
           else Just S.SimpleSrcSpan
             { S.startLine   = HSE.srcSpanStartLine srcSpan
             , S.startColumn = HSE.srcSpanStartColumn srcSpan
             , S.endLine     = HSE.srcSpanEndLine srcSpan
             , S.endColumn   = HSE.srcSpanEndColumn srcSpan
             }