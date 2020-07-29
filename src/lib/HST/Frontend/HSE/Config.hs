{-# LANGUAGE TypeFamilies #-}

module HST.Frontend.HSE.Config where

import qualified Language.Haskell.Exts         as HSE

import qualified HST.Frontend.Syntax           as S

-------------------------------------------------------------------------------
-- Type Family Instances                                                     --
-------------------------------------------------------------------------------

-- TODO Move type family instances to own @HST.Frontend.Syntax.HSE@ module.

-- | Wrapper for the fields of modules that are not supported.
data OriginalModuleHead = OriginalModuleHead
  { originalModuleHead    :: Maybe (HSE.ModuleHead HSE.SrcSpanInfo)
  , originalModulePragmas :: [HSE.ModulePragma HSE.SrcSpanInfo]
  , originalModuleImports :: [HSE.ImportDecl HSE.SrcSpanInfo]
  }
 deriving (Eq, Show)

-- | Type representing the AST data structure of @haskell-src-exts@.
--
--   Instantiates the type families for source spans, literals and type
--   expressions with the concrete types from @haskell-src-exts@. Also adds
--   instances for 'S.EqAST' and 'S.ShowAST' to allow the usage of @==@ and
--   @show@.
data HSE
type instance S.SrcSpanType HSE = HSE.SrcSpanInfo
type instance S.Literal HSE = HSE.Literal HSE.SrcSpanInfo
type instance S.TypeExp HSE = HSE.Type HSE.SrcSpanInfo
type instance S.OriginalModuleHead HSE = OriginalModuleHead
type instance S.OriginalDecl HSE = HSE.Decl HSE.SrcSpanInfo

instance S.EqAST HSE
instance S.ShowAST HSE
