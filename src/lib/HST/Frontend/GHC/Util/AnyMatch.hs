-- | This module contains a data type that is used for the transformation from
--   and to the @ghc-lib-parser@ AST.
--
--   In GHC-lib, the AST node for match groups is

module HST.Frontend.GHC.Util.AnyMatch where

import qualified GHC.Hs                            as GHC

import qualified HST.Frontend.Syntax as S
import  HST.Frontend.GHC.Config

-- | A data type that is used for the transformation of all nodes with pattern
--   matching, i.e., 'S.Match'es, 'S.Lambda' abstractions and 'S.Alt'ernatives.
data AnyMatch = AnyMatch
  { anyMatchSrcSpan  :: S.SrcSpan GHC
    -- ^ The source span of the match.
  , anyMatchContext  :: GHC.HsMatchContext (GHC.NameOrRdrName (GHC.IdP GHC.GhcPs))
    -- ^ Additional information about the match that has not been transfomed
    --   yet.
  , anyMatchPatterns :: [S.Pat GHC]
    -- ^ The patterns that are matched by this match.
  , anyMatchRhs      :: S.Rhs GHC
    -- ^ The right-hand side of the match.
  , anyMatchBinds    :: Maybe (S.Binds GHC)
    -- ^ Bindings of an options `where`-clause of the match.
  }
