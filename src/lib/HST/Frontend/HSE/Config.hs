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
--   for these types. Also adds instances for 'S.EqAST' and 'S.ShowAST' to
--   allow the usage of @==@ and @show@ for all AST components.
--
--   The front end is parameterized over the type of the source spans since
--   the AST of @haskell-src-exts@ is a functor that allows arbitrary
--   annotations and not just the source span data type provided by
--   @haskell-src-exts@ itself. Third-party programs that use the pattern
--   matching compiler as a library (such as the Free Compiler) can thus
--   use their own source spans.
data HSE (srcSpan :: *)

type instance S.SrcSpanType (HSE srcSpan) = srcSpan

type instance S.Literal (HSE srcSpan) = HSE.Literal srcSpan

type instance S.TypeExp (HSE srcSpan) = HSE.Type srcSpan

type instance S.OriginalModuleHead (HSE srcSpan) = OriginalModuleHead srcSpan

type instance S.OriginalDecl (HSE srcSpan) = HSE.Decl srcSpan

instance Eq srcSpan => S.EqAST (HSE srcSpan)

instance Show srcSpan => S.ShowAST (HSE srcSpan)

-------------------------------------------------------------------------------
-- Wrappers for @haskell-src-exts@ Types                                     --
-------------------------------------------------------------------------------
-- | Wrapper for the fields of modules that are not supported.
data OriginalModuleHead srcSpan = OriginalModuleHead
  { originalModuleHead    :: Maybe (HSE.ModuleHead srcSpan)
  , originalModulePragmas :: [HSE.ModulePragma srcSpan]
  , originalModuleImports :: [HSE.ImportDecl srcSpan]
  }
 deriving ( Eq, Show )
