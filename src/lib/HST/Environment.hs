-- | This module contains an abstract data type for the pattern matching
--   compiler's environment.
module HST.Environment
  ( -- * Environment Entries
    DataEntry(..)
    -- * Environment
  , Environment(..)
    -- * Lookup
  , lookupConEntries
  , lookupTypeName
  ) where

import qualified Data.Map.Strict        as Map
import           Data.Maybe             ( mapMaybe )

import           HST.Effect.InputModule
  ( ConEntry, ConName, ModuleInterface(..), TypeName )
import qualified HST.Frontend.Syntax    as S

-- | An entry of the 'Environment' for a data type whose constructors are in
--   scope.
data DataEntry a = DataEntry
  { dataEntryName :: TypeName a
    -- ^ The name of the data type.
  , dataEntryCons :: [ConName a]
    -- ^ The names of the data type's constructors.
  }

-------------------------------------------------------------------------------
-- Environment                                                               --
-------------------------------------------------------------------------------
-- | A data type for the environment of the pattern matching compiler
--   containing data types and data constructors currently in scope.
data Environment a = Environment
  { envCurrentModule :: ModuleInterface a
    -- ^ The module interface for the current module, containing its data types
    --   and data constructors.
  , envImportedModules :: [(S.ImportDecl a, ModuleInterface a)]
    -- ^ A list of all successfully imported module interfaces including their
    --   import declarations. 
  , envOtherEntries :: ModuleInterface a
    -- ^ A module interface containing all other data types and data
    --   constructors currently in scope.
  }

-------------------------------------------------------------------------------
-- Lookup                                                                    --
-------------------------------------------------------------------------------
-- | Looks up the constructors belonging to the the given data type name in the
--   given environment. The result includes the names of the modules the
--   constructors came from, if available.
--   
--   Returns an empty list if the data type name is not in scope.
--   Returns multiple module names and constructor lists if the data type name
--   is ambiguous.
lookupConEntries :: TypeName a -> Environment a -> [(Maybe (S.ModuleName a), [ConEntry a])]
lookupConEntries typeName env = mapMaybe
  (\x -> fmap ((,) (interfaceModName x)) (Map.lookup typeName (interfaceDataCons x)))
  (envCurrentModule env : envOtherEntries env : map snd (envImportedModules env))

-- | Looks up the data type names belonging to the the given data constructor
--   name in the given environment. The result includes the names of the
--   modules the types came from, if available.
--   
--   Returns an empty list if the data constructor name is not in scope.
--   Returns multiple module and data type names if the data constructor name
--   is ambiguous.
lookupTypeName :: ConName a -> Environment a -> [(Maybe (S.ModuleName a), TypeName a)]
lookupTypeName conName env = mapMaybe
  (\x -> fmap ((,) (interfaceModName x)) (Map.lookup conName (interfaceTypeNames x)))
  (envCurrentModule env : envOtherEntries env : map snd (envImportedModules env))
