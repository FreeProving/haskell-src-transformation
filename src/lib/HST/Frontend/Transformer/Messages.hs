-- | This module contains message building functions that can be used for the
--   transformation of ASTs.
module HST.Frontend.Transformer.Messages where

import           Polysemy          ( Member, Sem )

import           HST.Effect.Report
  ( Message(Message), Report, Severity(Error, Info), report, reportFatal )

-- | Reports a fatal error that the given feature is not supported.
notSupported
  :: Member Report r
  => String -- ^ The name of the feature (plural) that is not supported.
  -> Sem r b
notSupported feature
  = reportFatal $ Message Error $ feature ++ " are not supported!"

-- | Informs the user that the given feature is not supported and the
--   corresponding AST node will be skipped.
skipNotSupported
  :: Member Report r
  => String -- ^ The name of the feature (plural) that is not supported.
  -> Sem r ()
skipNotSupported feature = report
  $ Message Info
  $ feature ++ " are not supported and will be skipped!"
