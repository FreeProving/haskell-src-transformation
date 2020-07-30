-- | This module contains functions for transforming parsed Haskell modules
--   with the different front ends to the intermediate syntax and back.

module HST.Frontend.Transformer
  ( Transformable(..)
  )
where

import           Data.Composition               ( (.:) )
import           Polysemy                       ( Member
                                                , Sem
                                                )

import           HST.Effect.Report              ( Report )
import           HST.Frontend.FromGHC           ( GHC )
import qualified HST.Frontend.FromGHC          as FromGHC
import           HST.Frontend.FromHSE           ( HSE )
import qualified HST.Frontend.FromHSE          as FromHSE
import qualified HST.Frontend.ToGHC            as ToGHC
import qualified HST.Frontend.ToHSE            as ToHSE
import           HST.Frontend.Parser            ( ParsedModule
                                                  ( ParsedModuleGHC
                                                  , ParsedModuleHSE
                                                  )
                                                , getParsedModuleHSE
                                                , getParsedModuleGHC
                                                )
import qualified HST.Frontend.Syntax           as S

-- | Type class for "HST.Frontend.Syntax" configurations whose 'ParsedModule'
--   can be transformed to and from the intermediate syntax.
class Transformable a where
  -- | Transforms a parsed module to the intermediate syntax.
  transformModule
    :: Member Report r
    => ParsedModule a -- ^ The parsed module to transform.
    -> Sem r (S.Module a)

  -- | Transforms a module from the intermediate syntax back to a pretty
  --   printable module.
  unTransformModule
    :: Member Report r
    => ParsedModule a -- ^ The original input module. TODO remove me
    -> S.Module a     -- ^ The module to transform.
    -> Sem r (ParsedModule a)

instance Transformable HSE where
  transformModule = return . FromHSE.transformModule . getParsedModuleHSE
  unTransformModule =
    return . ParsedModuleHSE .: ToHSE.transformModule . getParsedModuleHSE

instance Transformable GHC where
  transformModule = return . FromGHC.transformModule . getParsedModuleGHC
  unTransformModule =
    return . ParsedModuleGHC .: ToGHC.transformModule . getParsedModuleGHC
