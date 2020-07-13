{-# LANGUAGE PackageImports, TypeFamilies #-}

module HST.Frontend.FromGHC where

import qualified "ghc-lib-parser" GHC.Hs       as GHC
import qualified "ghc-lib-parser" SrcLoc       as GHC

import qualified HST.Frontend.Syntax           as S

data GHC
type instance S.SrcSpanType GHC = GHC.SrcSpan
type instance S.Literal GHC = GHC.HsLit GHC.GhcPs
type instance S.TypeExp GHC = GHC.HsType GHC.GhcPs