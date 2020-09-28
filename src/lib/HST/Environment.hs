{-# LANGUAGE TupleSections #-}

-- | This module contains an abstract data type for the pattern matching
--   compiler's environment and functions for looking up entries in this
--   environment.
module HST.Environment
  ( -- * Environment
    Environment(..)
    -- * Lookup
  , lookupConEntries
  , lookupTypeName
  ) where

import           Data.Bifunctor            ( second )
import           Data.List                 ( find )
import           Data.Map.Strict           ( Map )
import qualified Data.Map.Strict           as Map
import           Data.Maybe                ( fromMaybe, mapMaybe )

import           HST.Effect.InputModule
  ( ConEntry(..), ConName, DataEntry(..), ModuleInterface(..), TypeName )
import qualified HST.Frontend.Syntax       as S

-------------------------------------------------------------------------------
-- Environment                                                               --
-------------------------------------------------------------------------------
-- | A data type for the environment of the pattern matching compiler
--   containing data types and data constructors currently in scope.
data Environment a = Environment
  { envCurrentModule   :: ModuleInterface a
    -- ^ The module interface for the current module, containing its data types
    --   and data constructors.
  , envImportedModules :: [([S.ImportDecl a], ModuleInterface a)]
    -- ^ A list of all successfully imported module interfaces including their
    --   import declarations. 
  , envOtherEntries    :: ModuleInterface a
    -- ^ A module interface containing all other data types and data
    --   constructors currently in scope.
  }

-------------------------------------------------------------------------------
-- Lookup                                                                    --
-------------------------------------------------------------------------------
-- | Looks up the data constructor entries belonging to the the given data type
--   name in the given environment and qualifies them so that they are
--   unambiguous, if possible. The result includes the names of the modules
--   the constructors came from, where available.
--
--   Returns an empty list if the data type name is not in scope.
--   Returns a single pair with the second entry being @Nothing@ if the data
--   type name is unambiguous, but not all of its constructors can be
--   identified unambiguously.
--   Returns multiple pairs of module names and constructor lists if the data
--   type name is ambiguous.
lookupConEntries :: TypeName a
                 -> Environment a
                 -> [(Maybe (S.ModuleName a), Maybe [ConEntry a])]
lookupConEntries typeName env = map
  (qualifyLookupResult (mapM . qualifyConEntry env) . second dataEntryCons)
  (lookupWith interfaceDataEntries typeName env)

-- | Looks up the data type names belonging to the the given data constructor
--   name in the given environment and qualifies them so that they are
--   unambiguous, if possible. The result includes the names of the modules the
--   types came from, where available.
--
--   Returns an empty list if the data constructor name is not in scope.
--   Returns a single pair with the second entry being @Nothing@ if the given
--   data constructor name is unambiguous, but the data type it belongs to
--   cannot be identified unambiguously.
--   Returns multiple pairs of module and data type names if the data
--   constructor name is ambiguous.
lookupTypeName :: ConName a
               -> Environment a
               -> [(Maybe (S.ModuleName a), Maybe (TypeName a))]
lookupTypeName conName env = map
  (qualifyLookupResult (qualifyQNameEnv interfaceDataEntries env)
   . second conEntryType) (lookupWith interfaceConEntries conName env)

-------------------------------------------------------------------------------
-- Lookup Utility Functions                                                  --
-------------------------------------------------------------------------------
-- | Looks up the given data type or constructor name in the maps gotten when
--   applying the given function to the module interfaces in the given
--   environment. In addition to the data type entries or constructor entries
--   found, the result includes the import declarations and the module
--   interfaces of the modules the found values came from.
lookupWith :: (ModuleInterface a -> Map (S.QName a) v)
           -> S.QName a
           -> Environment a
           -> [(([S.ImportDecl a], ModuleInterface a), v)]
lookupWith getMap qName env = mapMaybe
  (\x -> fmap (x, ) (Map.lookup (S.unQualifyQName qName) (getMap (snd x))))
  (possibleInterfaces qName env)

-- | Returns the list of all module interfaces in the given environment with
--   their lists of import declarations, that the given possibly qualified name
--   could refer to.
possibleInterfaces :: S.QName a
                   -> Environment a
                   -> [([S.ImportDecl a], ModuleInterface a)]
possibleInterfaces qName env =
  let defInterfaces    = filter (fitsToInterface qName . snd)
        [([], envCurrentModule env), ([], envOtherEntries env)]
      importInterfaces = filter (any (fitsToImport qName) . fst)
        (envImportedModules env)
  in defInterfaces ++ importInterfaces
 where
  -- | Checks if the given possibly qualified name could refer to an entry of
  --   the given module interface.
  --
  --   It is assumed that the given module interface is imported unqualified
  --   since it should either belong to the current module or implicitly
  --   imported modules. It therefore returns @True@ for names qualified with
  --   the module interface name and all unqualified names.
  fitsToInterface :: S.QName a -> ModuleInterface a -> Bool
  fitsToInterface (S.Qual _ modName _) = elem modName . interfaceModName
  fitsToInterface _                    = const True

  -- | Checks if the given possibly qualified name could refer to an entry of a
  --   module imported with the given import declaration.
  fitsToImport :: S.QName a -> S.ImportDecl a -> Bool
  fitsToImport (S.Qual _ modName _) = (==) modName . getImportQualifier
  fitsToImport _                    = not . S.importIsQual

-- | Gets the qualifier that identifiers referring to a module imported by the
--   given import declaration are possibly qualified by.
getImportQualifier :: S.ImportDecl a -> S.ModuleName a
getImportQualifier importDecl = fromMaybe (S.importModule importDecl)
  (S.importAsName importDecl)

-------------------------------------------------------------------------------
-- Qualification of Lookup Results                                           --
-------------------------------------------------------------------------------
-- | Uses the given qualification function (first argument) and the given
--   qualification information (first element of the second argument) to
--   qualify the given lookup result (second element of the second argument).
--
--   In addition to the qualified lookup result, the module name retrieved from
--   the given qualification information is returned.
qualifyLookupResult :: (Maybe (Bool, [S.ModuleName a]) -> b -> c)
                    -> (([S.ImportDecl a], ModuleInterface a), b)
                    -> (Maybe (S.ModuleName a), c)
qualifyLookupResult qualify (qualInfo@(_, interface), lookupResult)
  = ( interfaceModName interface
    , qualify (simplifyQualInfo qualInfo) lookupResult
    )
 where
  -- | Simplifies qualification information consisting of a module interface
  --   and a list of import declarations in the following way:
  --   If the qualification information does not specify a name, @Nothing@ is
  --   returned.
  --   Otherwise, a bool specifying whether identifiers must be qualified and
  --   the module names that could be used as a qualifier are returned.
  simplifyQualInfo :: ([S.ImportDecl a], ModuleInterface a)
                   -> Maybe (Bool, [S.ModuleName a])
  simplifyQualInfo ([], interface') = fmap ((False, ) . (:[]))
    (interfaceModName interface')
  simplifyQualInfo (imports, _)
    = Just (all S.importIsQual imports, map getImportQualifier imports)

-- | Qualifies the given data constructor entry based on the given
--   qualification information so that neither the data constructor name nor
--   the data type name of the constructor entry are ambiguous in the given
--   environment. Returns @Nothing@ if that is not possible.
qualifyConEntry :: Environment a
                -> Maybe (Bool, [S.ModuleName a])
                -> ConEntry a
                -> Maybe (ConEntry a)
qualifyConEntry env qualInfo conEntry = do
  conName <- qualifyQNameEnv interfaceConEntries env qualInfo
    (conEntryName conEntry)
  typeName <- qualifyQNameEnv interfaceDataEntries env qualInfo
    (conEntryType conEntry)
  return conEntry { conEntryName = conName, conEntryType = typeName }

-- | Qualifies the given possibly qualified name based on the given
--   qualification information so that it is not ambiguous in its namespace of
--   the given environment, where the namespace (data types or constructors) is
--   specified by the given function. Returns @Nothing@ if that is not possible.
qualifyQNameEnv :: (ModuleInterface a -> Map (S.QName a) v)
                -> Environment a
                -> Maybe (Bool, [S.ModuleName a])
                -> S.QName a
                -> Maybe (S.QName a)
qualifyQNameEnv getMap env qualInfo uqName
  = let mustBeQual          = maybe False fst qualInfo
        modNames            = maybe [] snd qualInfo
        qNames              = [uqName | not mustBeQual]
          ++ (qualifyQName uqName <$> modNames)
        isUnambigious qName = length (lookupWith getMap qName env) == 1
    in find isUnambigious qNames
 where
  -- | Qualifies the given unqualified 'S.QName' by the given module name.
  --
  --   Already qualified names and special names for built-in data constructors
  --   are returned as given.
  qualifyQName :: S.QName a -> S.ModuleName a -> S.QName a
  qualifyQName (S.UnQual s name) modName' = S.Qual s modName' name
  qualifyQName qName _                    = qName
