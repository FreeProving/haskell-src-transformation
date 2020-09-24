{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module defines an effect for computations that can store the file
--   path and the contents of multiple input files.
module HST.Effect.InputFile
  ( -- * Effect
    InputFile
    -- * Actions
  , getInputFile
    -- * Interpretations
  , runInputFile
  , runInputFileNoIO
  ) where

import           Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import           Polysemy
  ( Member, Sem, interpret, makeSem, reinterpret )
import           Polysemy.Embed  ( Embed, embed )
import           Polysemy.State  ( State, evalState, gets, modify )

-------------------------------------------------------------------------------
-- Effect and Actions                                                        --
-------------------------------------------------------------------------------
-- | An effect capable of getting the contents of input files knowing their
--   file path.
data InputFile m a where
  GetInputFile :: FilePath -> InputFile m String

makeSem ''InputFile

-------------------------------------------------------------------------------
-- Interpretations                                                           --
-------------------------------------------------------------------------------
-- | Handles a computation by providing an initially empty file map.
runInputFile :: Member (Embed IO) r => Sem (InputFile ': r) a -> Sem r a
runInputFile = runWithInputFile Map.empty

-- | Handles a computation by providing the supplied file map to get input
--   files from or to add new input files by asking for files that are not yet
--   in the map.
runWithInputFile :: Member (Embed IO) r
                 => Map FilePath String
                 -> Sem (InputFile ': r) a
                 -> Sem r a
runWithInputFile initialFileMap = evalState initialFileMap . inputFileToState
 where
  -- | Reinterprets 'InputFile' in terms of 'State'.
  inputFileToState :: Member (Embed IO) r
                   => Sem (InputFile ': r) a
                   -> Sem (State (Map FilePath String) ': r) a
  inputFileToState = reinterpret \case
    GetInputFile filePath -> do
      maybeContents <- gets $ Map.lookup filePath
      case maybeContents of
        Nothing       -> do
          contents <- embed $ readFile filePath
          modify $ Map.insert filePath contents
          return contents
        Just contents -> return contents

-- | Handles a computation by providing a map that maps file paths to contents.
--   This handler does not use IO actions but returns the empty string if the
--   given file was not found.
runInputFileNoIO :: Map FilePath String -> Sem (InputFile ': r) a -> Sem r a
runInputFileNoIO fileMap = interpret \case
  GetInputFile filePath ->
    let maybeContents = Map.lookup filePath fileMap
    in case maybeContents of
         Nothing       -> error ("File not found: " ++ filePath)
         Just contents -> return contents
