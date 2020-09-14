-- | This module contains a data type that is used for the transformation of
--   function declarations, lambda abstractions and @case@ alternatives from
--   and to the @ghc-lib-parser@ AST.
--
--   In the GHC AST, the nodes for matches are used not just for function
--   declarations but also for lambda abstractions and @case@ alternatives.
--   Additional information such as the name of the matched function is stored
--   in a 'GHC.HsMatchContext'. The remaining information is the same in every
--   case. In order to avoid code duplication, the AST transformation uses
--   the data type defined in this module to transform the parts of a match
--   that are not context specific.
module HST.Frontend.GHC.Util.AnyMatch ( AnyMatch(..) ) where

import qualified GHC.Hs                  as GHC

import           HST.Frontend.GHC.Config ( GHC )
import qualified HST.Frontend.Syntax     as S

-- | A data type that is used for the transformation of all nodes with pattern
--   matching, i.e., 'S.Match'es, 'S.Lambda' abstractions and 'S.Alt'ernatives.
data AnyMatch = AnyMatch
  { anyMatchSrcSpan  :: S.SrcSpan GHC
    -- ^ The source span of the match.
  , anyMatchContext
      :: GHC.HsMatchContext (GHC.NameOrRdrName (GHC.IdP GHC.GhcPs))
    -- ^ Additional information about the match that has not been transformed
    --   yet.
  , anyMatchPatterns :: [S.Pat GHC]
    -- ^ The patterns that are matched by this match.
  , anyMatchRhs      :: S.Rhs GHC
    -- ^ The right-hand side of the match.
  , anyMatchBinds    :: Maybe (S.Binds GHC)
    -- ^ Bindings of an optional @where@-clause of the match.
  }
