{-# LANGUAGE PackageImports #-}

-- | This module contains functions for pretty-printing modules that have
--   been converted back from the intermediate representation.

module HST.Frontend.PrettyPrinter
  ( PrettyPrintable(prettyPrintModule)
  )
where

import qualified "ghc-lib-parser" Outputable   as GHC
import qualified Language.Haskell.Exts         as HSE

import           HST.Frontend.FromGHC           ( GHC
                                                , defaultDynFlags
                                                )
import           HST.Frontend.FromHSE           ( HSE )
import           HST.Frontend.Parser

-- | Type class for "HST.Frontend.Syntax" configurations whose 'ParsedModule's
--   can be pretty-printed.
class PrettyPrintable a where
  -- | Pretty prints the given module.
  prettyPrintModule
    :: ParsedModule a -- ^ The module to pretty-print.
    -> String

-- | Pretty prints the given Haskell module with the pretty printer of
--   @haskell-src-exts@.
instance PrettyPrintable HSE where
  prettyPrintModule =
    HSE.prettyPrintStyleMode
        (HSE.Style { HSE.mode           = HSE.PageMode
                   , HSE.lineLength     = 120
                   , HSE.ribbonsPerLine = 1.5
                   }
        )
        HSE.defaultMode
      . unParsedModuleHSE

-- | Pretty prints the given Haskell module with the pretty printer of
--   @ghc-lib-parser@.
instance PrettyPrintable GHC where
  prettyPrintModule = GHC.showPpr defaultDynFlags . unParsedModuleGHC
