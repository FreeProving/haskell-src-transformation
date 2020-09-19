-- | This module contains variations of the "HST.Environment" lookup functions
--   that remove the list and pair constructor wrapping the return type.
--
--   If the original function does not return a singleton list, a fatal error
--   mentioning the out-of-scope or ambiguous entry is reported.
module HST.Environment.LookupOrReport
  ( lookupConEntriesOrReport
  , lookupTypeNameOrReport
  ) where

import           Data.List              ( intercalate )
import           Polysemy               ( Members, Sem )

import           HST.Effect.Env         ( Env, inEnv )
import           HST.Effect.InputModule ( ConEntry, ConName, TypeName )
import           HST.Effect.Report      ( Report, reportFatal )
import           HST.Environment        ( lookupConEntries, lookupTypeName )
import qualified HST.Frontend.Syntax    as S
import           HST.Util.Messages      ( Severity(Error), message )
import           HST.Util.PrettyName    ( prettyName )

-- | Looks up the constructors belonging to the the given data type name in the
--   current environment.
--
--   Reports a fatal error if the data type could not be found or if it could
--   be found in multiple modules. In the latter case, the names of
--   these modules are displayed as part of the error message.
lookupConEntriesOrReport
  :: Members '[Env a, Report] r => TypeName a -> Sem r [ConEntry a]
lookupConEntriesOrReport typeName = do
  conEntriess <- inEnv $ lookupConEntries typeName
  case conEntriess of
    []                -> reportFatal
      $ message Error (S.getSrcSpan typeName)
      $ "Data type not in scope: " ++ prettyName typeName
    [(_, Nothing)] -> reportFatal
      $ message Error (S.getSrcSpan typeName)
      $ "Constructor entries for the data type `"
      ++ prettyName typeName
      ++ "` could not be returned, because at least one of them cannot be "
      ++ "identified unambiguously."
    [(_, Just conEntries)] -> return conEntries
    _                 -> reportFatal
      $ message Error (S.getSrcSpan typeName)
      $ "Ambiguous data type `"
      ++ prettyName typeName
      ++ "` could refer to one of the following modules: "
      ++ displayModuleNames conEntriess

-- | Looks up the data type names belonging to the the given data constructor
--   name in the current environment.
--
--   Reports a fatal error if the data constructor could not be found or if it
--   could be found in multiple modules. In the latter case, the names of these
--   modules are displayed as part of the error message.
lookupTypeNameOrReport
  :: Members '[Env a, Report] r => ConName a -> Sem r (TypeName a)
lookupTypeNameOrReport conName = do
  typeNames <- inEnv $ lookupTypeName conName
  case typeNames of
    []              -> reportFatal
      $ message Error (S.getSrcSpan conName)
      $ "Data constructor not in scope: " ++ prettyName conName
    [(_, Nothing)] -> reportFatal
      $ message Error (S.getSrcSpan conName)
      $ "The name of the data type that the constructor `"
      ++ prettyName conName
      ++ "` belongs to could not be returned, because it cannot be identified "
      ++ "unambiguously."
    [(_, Just typeName)] -> return typeName
    _               -> reportFatal
      $ message Error (S.getSrcSpan conName)
      $ "Ambiguous data constructor `"
      ++ prettyName conName
      ++ "` could refer to one of the following modules: "
      ++ displayModuleNames typeNames

-- | Display the module names contained in the first pair entries of the given
--   list.
--
--   As no modules without a module name should be able to be imported
--   successfully, missing module names are replaced with @This module@.
displayModuleNames :: [(Maybe (S.ModuleName a), b)] -> String
displayModuleNames = intercalate ", "
  . map (maybe "This module" prettyName . fst)
