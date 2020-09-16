{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module defines an effect that allows to get modules and module
--   interfaces by the file path and module interfaces by the module name.
module HST.Effect.InputModule
  ( -- * Module Interface types
    ModuleInterface(..)
  , ConEntry(..)
  , TypeName
  , ConName
    -- * Effect
  , InputModule
    -- * Actions
  , getInputModule
  , getInputModuleInterface
  , getInputModuleInterfaceByName
    -- * Interpretations
  , runInputModule
  ) where

import           Data.Map.Strict     ( Map )
import qualified Data.Map.Strict     as Map
import           Data.Maybe          ( mapMaybe )
import           Polysemy            ( Member, Sem, makeSem, interpret )

import           HST.Effect.Report   ( Report, reportFatal )
import qualified HST.Frontend.Syntax as S
import           HST.Util.Messages   ( Severity(Error), message )

-------------------------------------------------------------------------------
-- Aliases for Names in Module Interfaces                                    --
-------------------------------------------------------------------------------
-- | The name of a data type.
type TypeName a = S.QName a

-- | The name of a data constructor.
type ConName a = S.QName a

-------------------------------------------------------------------------------
-- Module Interface Data Types                                               --
-------------------------------------------------------------------------------
-- | A data type for module interfaces that stores the name and the data types
--   of a module.
data ModuleInterface a = ModuleInterface
  { interfaceModName  :: Maybe (S.ModuleName a)
    -- ^ The name of the module or @Nothing@, if the module does not have a
    --   name.
  , interfaceDataCons :: Map (TypeName a) [ConEntry a]
    -- ^ A map that maps data types to their constructors.
  }

-- | An entry of the 'ModuleInterface' for a data constructor.
data ConEntry a = ConEntry
  { conEntryName    :: ConName a
    -- ^ The name of the constructor.
  , conEntryArity   :: Int
    -- ^ The number of fields of the constructor.
  , conEntryIsInfix :: Bool
    -- ^ Whether the constructor should be written in infix notation or not.
  , conEntryType    :: TypeName a
    -- ^ The name of the data type that the constructor belongs to.
  }

-------------------------------------------------------------------------------
-- Effect and Actions                                                        --
-------------------------------------------------------------------------------
-- | An effect allowing access to the ASTs and interfaces of transformed
--   modules by their file path. The interfaces can also be accessed by the
--   module name.
data InputModule a m b where
  GetInputModule :: FilePath -> InputModule a m (S.Module a)
  GetInputModuleInterface :: FilePath -> InputModule a m (ModuleInterface a)
  GetInputModuleInterfaceByName
    :: S.ModuleName a -> InputModule a m (Maybe (ModuleInterface a))

makeSem ''InputModule

-------------------------------------------------------------------------------
-- Interpretations                                                           --
-------------------------------------------------------------------------------
-- | Handles a computation by providing the supplied module map to get modules
--   or module interfaces from.
--
--   If one of the provided actions to get a module or interface by file path
--   is used and the module is not in the map, an error is reported.
--   If the provided action to get a module interface by module name is used
--   and the module name is not in the map, @Nothing@ is returned.
runInputModule :: Member Report r
               => [(FilePath, (S.Module a, ModuleInterface a))]
               -> Sem (InputModule a ': r) b
               -> Sem r b
runInputModule moduleList =
  let moduleMap = Map.fromList moduleList
      moduleNameMap =
        Map.fromList (mapMaybe createModuleNameMapEntry moduleList)
  in interpret \case
     GetInputModule filePath -> fmap fst (getInputEntry filePath moduleMap)
     GetInputModuleInterface filePath ->
       fmap snd (getInputEntry filePath moduleMap)
     GetInputModuleInterfaceByName modName ->
       case Map.lookup modName moduleNameMap of
         Just filePath -> Just <$> fmap snd (getInputEntry filePath moduleMap)
         Nothing -> return Nothing
 where
  -- | Looks up the given file path in the given module map and returns the
  --   corresponding entry or reports an error, if no entry is available.
  getInputEntry :: Member Report r
                => FilePath
                -> Map FilePath (S.Module a, ModuleInterface a)
                -> Sem r (S.Module a, ModuleInterface a)
  getInputEntry filePath moduleMap =
    case Map.lookup filePath moduleMap of
      Just (modul, interface) -> return (modul, interface)
      Nothing -> reportFatal $ message Error S.NoSrcSpan
                   $ "No input module was found for " ++ filePath ++ "!"
  -- | Creates an entry of a map which maps module names to file paths by
  --   converting a regular module map entry.
  createModuleNameMapEntry :: (FilePath, (S.Module a, ModuleInterface a))
                           -> Maybe (S.ModuleName a, FilePath)
  createModuleNameMapEntry (filePath, (S.Module _ _ (Just modName) _ _, _))
   = Just (modName, filePath)
  createModuleNameMapEntry _ = Nothing
