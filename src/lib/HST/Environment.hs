{-# LANGUAGE TupleSections #-}

-- | This module contains an abstract data type for the pattern matching
--   compiler's environment.
module HST.Environment
  ( -- * Environment
    Environment(..)
    -- * Lookup
  , lookupConEntries
  , lookupTypeName
  ) where

import qualified Data.Map.Strict        as Map
import           Data.Maybe             ( fromMaybe, mapMaybe )

import           HST.Effect.InputModule
  ( ConEntry, ConName, ModuleInterface(..), TypeName )
import qualified HST.Frontend.Syntax    as S

-------------------------------------------------------------------------------
-- Environment                                                               --
-------------------------------------------------------------------------------
-- | A data type for the environment of the pattern matching compiler
--   containing data types and data constructors currently in scope.
data Environment a = Environment
  { envCurrentModule   :: ModuleInterface a
    -- ^ The module interface for the current module, containing its data types
    --   and data constructors.
  , envImportedModules :: [(S.ImportDecl a, ModuleInterface a)]
    -- ^ A list of all successfully imported module interfaces including their
    --   import declarations. 
  , envOtherEntries    :: ModuleInterface a
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
lookupConEntries
  :: TypeName a -> Environment a -> [(Maybe (S.ModuleName a), [ConEntry a])]
lookupConEntries typeName env = mapMaybe
  (\x ->
   fmap (interfaceModName x, ) (Map.lookup typeName (interfaceDataCons x)))
  (envCurrentModule env
   : envOtherEntries env
   : map snd (envImportedModules env))

-- | Looks up the data type names belonging to the the given data constructor
--   name in the given environment. The result includes the names of the
--   modules the types came from, if available.
--
--   Returns an empty list if the data constructor name is not in scope.
--   Returns multiple module and data type names if the data constructor name
--   is ambiguous.
lookupTypeName
  :: ConName a -> Environment a -> [(Maybe (S.ModuleName a), TypeName a)]
lookupTypeName conName env = mapMaybe
  (\x ->
   fmap (interfaceModName x, ) (Map.lookup conName (interfaceTypeNames x)))
  (envCurrentModule env
   : envOtherEntries env
   : map snd (envImportedModules env))

-------------------------------------------------------------------------------
-- Lookup Utility Functions                                                  --
-------------------------------------------------------------------------------
-- | Checks if the given qualified name could refer to an entry of the given
--   module interface.
--
--   It is assumed that the given module interface is imported unqualified
--   since it should either belong to the current module or implicitly imported
--   modules. It therefore returns @True@ for names qualified with the module
--   interface name and all unqualified names.
fitsToInterface :: S.QName a -> ModuleInterface a -> Bool
fitsToInterface (S.Qual _ modName _) = elem modName . interfaceModName
fitsToInterface _ = const True

-- | Checks if the given qualified name could refer to an entry of a module
--   imported with the given import declaration.
fitsToImport :: S.QName a -> S.ImportDecl a -> Bool
fitsToImport (S.Qual _ modName _) importDecl = modName ==
  fromMaybe (S.importModule importDecl) (S.importAsName importDecl)
fitsToImport _ importDecl = not (S.importIsQual importDecl)

-- | Removes the possible qualification of the given 'S.QName'.
--
--   Other 'S.QName's, including special names for built-in data constructors
--   are returned as given.
unQualifyName :: S.QName a -> S.QName a
unQualifyName (S.Qual s _ name) = S.UnQual s name
unQualifyName uqName = uqName
