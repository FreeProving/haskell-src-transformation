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

import           Data.Map.Strict        ( Map )
import qualified Data.Map.Strict        as Map
import           Data.Maybe             ( fromMaybe, mapMaybe )

import           HST.Effect.InputModule
  ( ConEntry(..), ConName, ModuleInterface(..), TypeName )
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
  :: TypeName a -> Environment a -> [(Maybe (S.ModuleName a), Maybe [ConEntry a])]
lookupConEntries typeName env =
  map (qualifyLookupResult (\x -> sequence . map (qualifyConEntry env x)))
      (lookupWith interfaceDataCons typeName env)

-- | Looks up the data type names belonging to the the given data constructor
--   name in the given environment. The result includes the names of the
--   modules the types came from, if available.
--
--   Returns an empty list if the data constructor name is not in scope.
--   Returns multiple module and data type names if the data constructor name
--   is ambiguous.
lookupTypeName
  :: ConName a -> Environment a -> [(Maybe (S.ModuleName a), Maybe (TypeName a))]
lookupTypeName conName env =
  map (qualifyLookupResult (qualifyQNameEnv interfaceDataCons env))
      (lookupWith interfaceTypeNames conName env)

-------------------------------------------------------------------------------
-- Lookup Utility Functions                                                  --
-------------------------------------------------------------------------------
-- TODO
-- | A generic version of 'lookupConEntries' and 'lookupTypeName' that
--   additionally takes a function mapping a module interface to the desired
--   map, i. e. one of the field names of 'ModuleInterface', as its first
--   argument.
lookupWith :: (ModuleInterface a -> Map (S.QName a) v)
           -> S.QName a
           -> Environment a
           -> [((Maybe (S.ImportDecl a), ModuleInterface a), v)]
lookupWith getMap qName env = mapMaybe
  (\x -> fmap (x, ) (Map.lookup (unQualifyName qName) (getMap (snd x))))
  (possibleInterfaces qName env)

-- TODO
-- | Returns the list of all module interfaces in the given environment the
--   given possibly qualified name could refer to.
possibleInterfaces
  :: S.QName a -> Environment a -> [(Maybe (S.ImportDecl a), ModuleInterface a)]
possibleInterfaces qName env = filter (fitsToInterface qName . snd)
  [(Nothing, envCurrentModule env), (Nothing, envOtherEntries env)]
  ++ map (\(x, y) -> (Just x, y))
         (filter (fitsToImport qName . fst) (envImportedModules env))

-- | Checks if the given possibly qualified name could refer to an entry of the
--   given module interface.
--
--   It is assumed that the given module interface is imported unqualified
--   since it should either belong to the current module or implicitly imported
--   modules. It therefore returns @True@ for names qualified with the module
--   interface name and all unqualified names.
fitsToInterface :: S.QName a -> ModuleInterface a -> Bool
fitsToInterface (S.Qual _ modName _) = elem modName . interfaceModName
fitsToInterface _                    = const True

-- | Checks if the given possibly qualified name could refer to an entry of a
--   module imported with the given import declaration.
fitsToImport :: S.QName a -> S.ImportDecl a -> Bool
fitsToImport (S.Qual _ modName _) = (==) modName . getImportQualifier
fitsToImport _ = not . S.importIsQual

getImportQualifier :: S.ImportDecl a -> S.ModuleName a
getImportQualifier importDecl =
  fromMaybe (S.importModule importDecl) (S.importAsName importDecl)

-- | Removes the possible qualification of the given 'S.QName'.
--
--   Other 'S.QName's, including special names for built-in data constructors,
--   are returned as given.
unQualifyName :: S.QName a -> S.QName a
unQualifyName (S.Qual s _ name) = S.UnQual s name
unQualifyName uqName            = uqName

qualifyLookupResult :: (Maybe (Bool, S.ModuleName a) -> b -> c)
                    -> ((Maybe (S.ImportDecl a), ModuleInterface a), b)
                    -> (Maybe (S.ModuleName a), c)
qualifyLookupResult qualify (qualInfo@(_, interface), lookupResult) =
  (interfaceModName interface, qualify (transformQualInfo qualInfo) lookupResult)
 where
  transformQualInfo :: (Maybe (S.ImportDecl a), ModuleInterface a)
                    -> Maybe (Bool, S.ModuleName a)
  transformQualInfo (Just importDecl, _) =
    Just (S.importIsQual importDecl, getImportQualifier importDecl)
  transformQualInfo (Nothing, interface') =
    fmap (False, ) (interfaceModName interface')

qualifyConEntry
  :: Environment a -> Maybe (Bool, S.ModuleName a) -> ConEntry a -> Maybe (ConEntry a)
qualifyConEntry env qualInfo conEntry =
  case ( qualifyQNameEnv interfaceTypeNames env qualInfo (conEntryName conEntry)
       , qualifyQNameEnv interfaceDataCons env qualInfo (conEntryType conEntry)
       ) of
    (Just conName, Just typeName) -> Just conEntry { conEntryName = conName
                                                   , conEntryType = typeName
                                                   }
    _ -> Nothing

qualifyQNameEnv :: (ModuleInterface a -> Map (S.QName a) v)
                -> Environment a
                -> Maybe (Bool, S.ModuleName a)
                -> S.QName a
                -> Maybe (S.QName a)
qualifyQNameEnv getMap env Nothing uqName = 
  if length (lookupWith getMap uqName env) == 1 then Just uqName else Nothing
qualifyQNameEnv getMap env (Just (mustBeQual, modName)) uqName =
  if not mustBeQual && length (lookupWith getMap uqName env) == 1
    then Just uqName
    else let qName = qualifyQName uqName modName
         in if length (lookupWith getMap qName env) == 1
              then Just qName
              else Nothing

qualifyQName :: S.QName a -> S.ModuleName a -> S.QName a
qualifyQName (S.UnQual s name) modName = S.Qual s modName name
qualifyQName qName _ = qName
