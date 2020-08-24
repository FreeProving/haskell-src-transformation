{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module defines an effect for computations that can store the file
--   path and the content of multiple input files.
module HST.Effect.InputFile
  ( -- * Effect
    InputFile
    -- * Actions
  , addInputFile
  , getInputFile
    -- * Interpretations
  , runInputFile
  ) where

import           Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import           Polysemy        ( Sem, makeSem, reinterpret )
import           Polysemy.State  ( State, evalState, get, put )

-------------------------------------------------------------------------------
-- Effect and Actions                                                        --
-------------------------------------------------------------------------------
-- | An effect capable of adding and getting the file path and the content of
--   input files.
--
--   While setting an input file, the content is converted to lines of code.
data InputFile m a where
  AddInputFile :: FilePath -> String -> InputFile m ()
  GetInputFile :: FilePath -> InputFile m (Maybe [String])

makeSem ''InputFile

-------------------------------------------------------------------------------
-- Interpretations                                                           --
-------------------------------------------------------------------------------
-- | Handles a computation by providing an initially empty file map.
runInputFile :: Sem (InputFile ': r) a -> Sem r a
runInputFile = runWithInputFile Map.empty

-- | Handles a computation by providing the supplied file map to read from or
--   to add new input files.
runWithInputFile :: Map FilePath [String] -> Sem (InputFile ': r) a -> Sem r a
runWithInputFile initialFileMap = evalState initialFileMap . inputFileToState
 where
  -- | Reinterprets 'InputFile' in terms of 'State'.
  inputFileToState
    :: Sem (InputFile ': r) a -> Sem (State (Map FilePath [String]) ': r) a
  inputFileToState = reinterpret \case
    AddInputFile path content -> get >>= put . Map.insert path (lines content)
    GetInputFile path         -> get >>= return . Map.lookup path
