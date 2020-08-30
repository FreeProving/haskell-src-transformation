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
  , getInputFile
    -- * Interpretations
  , runInputFile
  ) where

import           Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import           Polysemy        ( Member, Sem, makeSem, reinterpret )
import           Polysemy.Embed  ( Embed, embed )
import           Polysemy.Reader ( Reader, asks, runReader )

-------------------------------------------------------------------------------
-- Effect and Actions                                                        --
-------------------------------------------------------------------------------
-- | An effect capable of getting the content of input files knowing their file
--   path.
data InputFile m a where
  GetInputFile :: FilePath -> InputFile m (Maybe String)

makeSem ''InputFile

-------------------------------------------------------------------------------
-- Interpretations                                                           --
-------------------------------------------------------------------------------
-- | Handles a computation by reading the given files and providing a file map
--   with their file paths and contents.
runInputFile
  :: Member (Embed IO) r => [FilePath] -> Sem (InputFile ': r) a -> Sem r a
runInputFile inputFilePaths sem = do
  inputFileContents <- mapM (embed . readFile) inputFilePaths
  runWithInputFile (Map.fromList (zip inputFilePaths inputFileContents)) sem

-- | Handles a computation by providing the supplied file map to read from.
runWithInputFile :: Map FilePath String -> Sem (InputFile ': r) a -> Sem r a
runWithInputFile initialFileMap = runReader initialFileMap . inputFileToReader
 where
  -- | Reinterprets 'InputFile' in terms of 'Reader'.
  inputFileToReader
    :: Sem (InputFile ': r) a -> Sem (Reader (Map FilePath String) ': r) a
  inputFileToReader = reinterpret \case
    GetInputFile path -> asks $ Map.lookup path
