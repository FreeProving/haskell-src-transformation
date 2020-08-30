-- | This module contains message building functions that can be used for the
--   transformation of ASTs.
module HST.Frontend.Transformer.Messages where

import           Data.Char            ( isSpace )
import           Polysemy             ( Member, Members, Sem )

import           HST.Effect.InputFile ( InputFile, getInputFile )
import           HST.Effect.Report
  ( Message(Message), Report, Severity(Error, Info), report, reportFatal )
import qualified HST.Frontend.Syntax  as S

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

-- | Reports a fatal error that the given feature is not supported. Displays an
--   excerpt of the input code specified by the given source span.
notSupportedWithExcerpt
  :: Members '[InputFile, Report] r
  => String      -- ^ The name of the feature (plural) that is not supported.
  -> S.SrcSpan a -- ^ The source span of the excerpt to display.
  -> Sem r b
notSupportedWithExcerpt feature srcSpan = do
  excerpt <- displayCodeExcerpt srcSpan
  reportFatal $ Message Error $ feature ++ " are not supported!\n" ++ excerpt

-- | Informs the user that the given feature is not supported and the
--   corresponding AST node will be skipped. Displays an excerpt of the input
--   code specified by the given source span.
skipNotSupportedWithExcerpt
  :: Members '[InputFile, Report] r
  => String      -- ^ The name of the feature (plural) that is not supported.
  -> S.SrcSpan a -- ^ The source span of the excerpt to display.
  -> Sem r ()
skipNotSupportedWithExcerpt feature srcSpan = do
  excerpt <- displayCodeExcerpt srcSpan
  report
    $ Message Info
    $ feature ++ " are not supported and will be skipped!\n" ++ excerpt

