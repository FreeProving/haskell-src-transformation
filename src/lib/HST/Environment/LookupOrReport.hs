-- | This module contains variations of the "HST.Environment" lookup functions
--   that don't return @Maybe@ values but report a fatal error if a data or
--   type constructor cannot be found in the environment.
module HST.Environment.LookupOrReport
  ( lookupConEntryOrReport
  , lookupDataEntryOrReport
  ) where

import           Polysemy            ( Members, Sem )

import           HST.Effect.Env      ( Env, inEnv )
import           HST.Effect.InputModule ( ConEntry )
import           HST.Effect.Report   ( Report, reportFatal )
import           HST.Environment
  ( DataEntry, lookupConEntry, lookupDataEntry )
import qualified HST.Frontend.Syntax as S
import           HST.Util.Messages   ( Severity(Error), message )
import           HST.Util.PrettyName ( prettyName )

-- | Looks up the entry of a data constructor with the given name in the
--   current environment.
--
--   Reports a fatal error if the constructor could not be found.
lookupConEntryOrReport
  :: Members '[Env a, Report] r => S.QName a -> Sem r (ConEntry a)
lookupConEntryOrReport conName = do
  maybeConEntry <- inEnv $ lookupConEntry conName
  case maybeConEntry of
    Nothing       -> reportFatal
      $ message Error S.NoSrcSpan
      $ "Data constructor not in scope: " ++ prettyName conName
    Just conEntry -> return conEntry

-- | Looks up the entry of a type constructor with the given name in the
--   current environment.
--
--   Reports a fatal error if the constructor could not be found.
lookupDataEntryOrReport
  :: Members '[Env a, Report] r => S.QName a -> Sem r (DataEntry a)
lookupDataEntryOrReport dataName = do
  maybeDataEntry <- inEnv $ lookupDataEntry dataName
  case maybeDataEntry of
    Nothing        -> reportFatal
      $ message Error S.NoSrcSpan
      $ "Type constructor not in scope: " ++ prettyName dataName
    Just dataEntry -> return dataEntry
