-- | This module contains functions for transforming parsed Haskell modules
--   with the different front ends to the intermediate syntax and back.
module HST.Frontend.Transformer ( Transformable(..) ) where

import           Polysemy                ( Member, Sem )

import           HST.Effect.Report       ( Report )
import           HST.Frontend.GHC.Config ( GHC )
import qualified HST.Frontend.GHC.From   as FromGHC
import qualified HST.Frontend.GHC.To     as ToGHC
import           HST.Frontend.HSE.Config ( HSE )
import qualified HST.Frontend.HSE.From   as FromHSE
import qualified HST.Frontend.HSE.To     as ToHSE
import           HST.Frontend.Parser
  ( ParsedModule(ParsedModuleGHC, ParsedModuleHSE), getParsedModuleGHC
  , getParsedModuleHSE )
import qualified HST.Frontend.Syntax     as S

-- | Type class for "HST.Frontend.Syntax" configurations whose 'ParsedModule'
--   can be transformed to and from the intermediate syntax.
class Transformable a where
  -- | Transforms a parsed module to the intermediate syntax.
  transformModule :: Member Report r
                  => ParsedModule a -- ^ The parsed module to transform.
                  -> Sem r (S.Module a)

  -- | Transforms a module from the intermediate syntax back to a pretty
  --   printable module.
  unTransformModule :: Member Report r
                    => S.Module a     -- ^ The module to transform.
                    -> Sem r (ParsedModule a)

instance Transformable HSE where
  transformModule   = FromHSE.transformModule . getParsedModuleHSE

  unTransformModule = return . ParsedModuleHSE . ToHSE.transformModule

instance Transformable GHC where
  transformModule   = FromGHC.transformModule . getParsedModuleGHC

  unTransformModule = fmap ParsedModuleGHC . ToGHC.transformModule
