{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module defines an effect that allows to get modules and module
--   interfaces by the module name.
module HST.Effect.InputModule
  ( ModuleInterface(..)
  , InputModule
  , runInputModule
  ) where

import           Data.Map.Strict     ( Map )
import qualified Data.Map.Strict     as Map
import           Polysemy            ( Member, Members, Sem, interpret )
import           Polysemy.Reader     ( Reader, asks, runReader )

import           HST.Environment
import qualified HST.Frontend.Syntax as S

type TypeName a = S.QName a

type ConName a = S.QName a

-- | A data type for the module interface.
data ModuleInterface a = ModuleInterface
  { interfaceModName  :: S.ModuleName a               -- ^ The name of the module.
  , interfaceDataCons :: Map (TypeName a) [ConName a] -- ^ A map that maps types to its constructors
  }

data InputModule a m b where
  GetInputModule :: S.ModuleName a -> InputModule a m (Maybe (S.Module a))
  GetInputModuleInterface :: S.ModuleName a
    -> InputModule a m (Maybe (ModuleInterface a))

runInputModule :: Map (S.ModuleName a) (S.Module a, ModuleInterface a)
               -> Sem (InputModule a ': r) b
               -> Sem r b
runInputModule modules = interpret \case
  GetInputModule modName          -> return
    (fmap fst (Map.lookup modName modules))
  GetInputModuleInterface modName -> return
    (fmap snd (Map.lookup modName modules))
