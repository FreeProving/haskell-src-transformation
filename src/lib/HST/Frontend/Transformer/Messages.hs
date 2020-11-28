-- | This module contains message building functions that can be used for the
--   transformation of ASTs.
module HST.Frontend.Transformer.Messages where

import           Polysemy            ( Member, Sem )

import           HST.Effect.Report   ( Report, report, reportFatal )
import qualified HST.Frontend.Syntax as S
import           HST.Util.Messages   ( Severity(Error, Info), message )

-- | Reports a fatal error that the given feature is not supported. Displays an
--   excerpt of the input code specified by the given source span.
notSupported
  :: Member Report r
  => String      -- ^ The name of the feature (plural) that is not supported.
  -> S.SrcSpan a -- ^ The source span of the excerpt to display.
  -> Sem r b
notSupported feature srcSpan
  = reportFatal $ message Error srcSpan $ feature ++ " are not supported!"

-- | Informs the user that the given feature is not supported and the
--   corresponding AST node will be skipped. Displays an excerpt of the input
--   code specified by the given source span.
skipNotSupported
  :: Member Report r
  => String      -- ^ The name of the feature (plural) that is not supported.
  -> S.SrcSpan a -- ^ The source span of the excerpt to display.
  -> Sem r ()
skipNotSupported feature srcSpan
  = skipNotSupported' feature "will be skipped" srcSpan

-- | Like 'skipNotSupported' but has an additional parameter that specifies how
--   the AST node will be skipped.
skipNotSupported'
  :: Member Report r
  => String      -- ^ The name of the feature (plural) that is not supported.
  -> String      -- ^ The strategy that is applied to skip the node.
  -> S.SrcSpan a -- ^ The source span of the excerpt to display.
  -> Sem r ()
skipNotSupported' feature strategy srcSpan = report
  $ message Info srcSpan
  $ feature ++ " are not supported and " ++ strategy ++ "!"
