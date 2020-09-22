-- | This module contains functions for pretty-printing modules and expressions
--   that have been converted back from the intermediate representation.
module HST.Frontend.PrettyPrinter
  ( PrettyPrintable(prettyPrintModule, prettyPrintExp)
  ) where

import qualified Language.Haskell.Exts    as HSE
import qualified Outputable               as GHC
import           Polysemy                 ( Member, Sem )

import           HST.Effect.Report        ( Report )
import           HST.Frontend.GHC.Config  ( GHC, defaultDynFlags )
import           HST.Frontend.HSE.Config  ( HSE )
import qualified HST.Frontend.HSE.From    as FromHSE
import qualified HST.Frontend.HSE.To      as ToHSE
import qualified HST.Frontend.Syntax      as S
import           HST.Frontend.Transformer
  ( Transformable(..), getExpGHC, getExpHSE, getModuleGHC, getModuleHSE )

-- | Type class for "HST.Frontend.Syntax" configurations for which 'S.Module's
--   and 'S.Exp'ressions can be pretty-printed.
--
--   Modules are always transformed back into the front end specific
--   representation before they are pretty printed.
class Transformable a => PrettyPrintable a where
  -- | Pretty prints the given module.
  prettyPrintModule :: Member Report r => S.Module a -> Sem r String

  -- | Pretty prints the given expression.
  prettyPrintExp :: Member Report r => S.Exp a -> Sem r String

-- | Pretty prints the given Haskell module or expression with the pretty
--   printer of @haskell-src-exts@.
instance (FromHSE.TransformSrcSpan srcSpan, ToHSE.TransformSrcSpan srcSpan)
  => PrettyPrintable (HSE srcSpan) where
  prettyPrintModule = fmap
    (HSE.prettyPrintStyleMode styleHSE HSE.defaultMode . getModuleHSE)
    . unTransformModule

  prettyPrintExp    = fmap
    (HSE.prettyPrintStyleMode styleHSE HSE.defaultMode . getExpHSE)
    . unTransformExp

-- | The pretty-printing style used with the @haskell-src-exts@ front end.
styleHSE :: HSE.Style
styleHSE = HSE.Style { HSE.mode           = HSE.PageMode
                     , HSE.lineLength     = 80
                     , HSE.ribbonsPerLine = 1.0
                     }

-- | Pretty prints the given Haskell module or expression with the pretty
--   printer of @ghc-lib-parser@.
instance PrettyPrintable GHC where
  prettyPrintModule = fmap (GHC.showPpr defaultDynFlags . getModuleGHC)
    . unTransformModule

  prettyPrintExp    = fmap (GHC.showPpr defaultDynFlags . getExpGHC)
    . unTransformExp
