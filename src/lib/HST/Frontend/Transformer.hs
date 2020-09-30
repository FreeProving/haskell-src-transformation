{-# LANGUAGE TypeFamilies #-}

-- | This module contains functions for transforming parsed Haskell modules and
--   expressions with the different front ends to the intermediate syntax and
--   back.
module HST.Frontend.Transformer
  ( Transformable(transformModule, unTransformModule, transformExp,
              unTransformExp)
  , ModuleType(ModuleHSE, ModuleGHC)
  , ExpType(ExpHSE, ExpGHC)
  , getModuleHSE
  , getModuleGHC
  , getExpHSE
  , getExpGHC
  ) where

import qualified GHC.Hs                  as GHC
import qualified Language.Haskell.Exts   as HSE
import           Polysemy                ( Member, Sem )
import qualified SrcLoc                  as GHC

import           HST.Effect.Report       ( Report )
import           HST.Frontend.GHC.Config ( GHC )
import qualified HST.Frontend.GHC.From   as FromGHC
import qualified HST.Frontend.GHC.To     as ToGHC
import           HST.Frontend.HSE.Config ( HSE )
import qualified HST.Frontend.HSE.From   as FromHSE
import qualified HST.Frontend.HSE.To     as ToHSE
import qualified HST.Frontend.Syntax     as S

-- | Type class for "HST.Frontend.Syntax" configurations for which 'S.Module's
--   and 'S.Exp'ressions can be transformed to and from the intermediate
--   syntax.
class Transformable a where
  -- | Type family for the argument and return type of 'transformModule'
  --   and 'unTransformModule', respectively.
  data ModuleType a :: *

  -- | Type family for the argument and return type of 'transformExp'
  --   and 'unTransformExp', respectively.
  data ExpType a :: *

  -- | Transforms a parsed module to the intermediate syntax.
  transformModule :: Member Report r => ModuleType a -> Sem r (S.Module a)

  -- | Transforms a module from the intermediate syntax back to a pretty
  --   printable module.
  unTransformModule :: Member Report r => S.Module a -> Sem r (ModuleType a)

  -- | Transforms a parsed expression to the intermediate syntax.
  transformExp :: Member Report r => ExpType a -> Sem r (S.Exp a)

  -- | Transforms an expression from the intermediate syntax back to a pretty
  --   printable expression.
  unTransformExp :: Member Report r => S.Exp a -> Sem r (ExpType a)

instance (ToHSE.TransformSrcSpan srcSpan, FromHSE.TransformSrcSpan srcSpan)
  => Transformable (HSE srcSpan) where
  data ModuleType (HSE srcSpan)
    = ModuleHSE { getModuleHSE :: HSE.Module srcSpan }

  data ExpType (HSE srcSpan) = ExpHSE { getExpHSE :: HSE.Exp srcSpan }

  transformModule = FromHSE.transformModule . getModuleHSE

  unTransformModule = return . ModuleHSE . ToHSE.transformModule

  transformExp = FromHSE.transformExp . getExpHSE

  unTransformExp = return . ExpHSE . ToHSE.transformExp

instance Transformable GHC where
  data ModuleType GHC
    = ModuleGHC { getModuleGHC :: GHC.Located (GHC.HsModule GHC.GhcPs) }

  data ExpType GHC = ExpGHC { getExpGHC :: GHC.Located (GHC.HsExpr GHC.GhcPs) }

  transformModule = FromGHC.transformModule . getModuleGHC

  unTransformModule = fmap ModuleGHC . ToGHC.transformModule

  transformExp = FromGHC.transformExpr . getExpGHC

  unTransformExp = fmap ExpGHC . ToGHC.transformExp
