-- | This module contains functions for pretty-printing modules and expressions
--   that have been converted back from the intermediate representation.
module HST.Frontend.PrettyPrinter
  ( PrettyPrintable(prettyPrintModule, prettyPrintExp)
  ) where

import qualified Language.Haskell.Exts   as HSE
import qualified Outputable              as GHC

import           HST.Frontend.GHC.Config ( GHC, defaultDynFlags )
import           HST.Frontend.HSE.Config ( HSE )
import           HST.Frontend.Parser
  ( ParsedExp, ParsedModule, getParsedExpGHC
  , getParsedExpHSE, getParsedModuleGHC, getParsedModuleHSE )

-- | Type class for "HST.Frontend.Syntax" configurations whose 'ParsedModule's
--   and 'ParsedExp's can be pretty-printed.
class PrettyPrintable a where
  -- | Pretty prints the given module.
  prettyPrintModule :: ParsedModule a -- ^ The module to pretty-print.
                    -> String

  -- | Pretty prints the given expression.
  prettyPrintExp :: ParsedExp a -> String

-- | Pretty prints the given Haskell module or expression with the pretty
--   printer of @haskell-src-exts@.
instance PrettyPrintable HSE where
  prettyPrintModule
    = HSE.prettyPrintStyleMode styleHSE HSE.defaultMode . getParsedModuleHSE

  prettyPrintExp = HSE.prettyPrintStyleMode styleHSE HSE.defaultMode
    . getParsedExpHSE

-- | The pretty-printing style used with the @haskell-src-exts@ front end.
styleHSE :: HSE.Style
styleHSE = HSE.Style { HSE.mode           = HSE.PageMode
                     , HSE.lineLength     = 80
                     , HSE.ribbonsPerLine = 1.0
                     }

-- | Pretty prints the given Haskell module or expression with the pretty
--   printer of @ghc-lib-parser@.
instance PrettyPrintable GHC where
  prettyPrintModule     = GHC.showPpr defaultDynFlags . getParsedModuleGHC

  prettyPrintExp = GHC.showPpr defaultDynFlags . getParsedExpGHC
