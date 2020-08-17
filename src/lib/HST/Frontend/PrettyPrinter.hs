-- | This module contains functions for pretty-printing modules that have
--   been converted back from the intermediate representation.
module HST.Frontend.PrettyPrinter ( PrettyPrintable(prettyPrintModule) ) where

import qualified Language.Haskell.Exts as HSE
import qualified Outputable as GHC

import           HST.Frontend.GHC.Config ( GHC, defaultDynFlags )
import           HST.Frontend.HSE.Config ( HSE )
import           HST.Frontend.Parser
  ( ParsedModule, getParsedModuleGHC, getParsedModuleHSE )

-- | Type class for "HST.Frontend.Syntax" configurations whose 'ParsedModule's
--   can be pretty-printed.
class PrettyPrintable a where
  -- | Pretty prints the given module.
  prettyPrintModule :: ParsedModule a -- ^ The module to pretty-print.
                    -> String

-- | Pretty prints the given Haskell module with the pretty printer of
--   @haskell-src-exts@.
instance PrettyPrintable HSE where
  prettyPrintModule = HSE.prettyPrintStyleMode
    (HSE.Style { HSE.mode           = HSE.PageMode
               , HSE.lineLength     = 80
               , HSE.ribbonsPerLine = 1.0
               }) HSE.defaultMode
    . getParsedModuleHSE

-- | Pretty prints the given Haskell module with the pretty printer of
--   @ghc-lib-parser@.
instance PrettyPrintable GHC where
  prettyPrintModule = GHC.showPpr defaultDynFlags . getParsedModuleGHC
