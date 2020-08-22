{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module defines an effect for computations that can store the file
--   path and the content of an input file.
module HST.Effect.InputFile
  ( -- * Effect
    InputFile
    -- * Actions
  , setFile
  , getFile
    -- * Interpretations
  , runInputFile
  ) where

import           Polysemy        ( Sem, makeSem, reinterpret )
import           Polysemy.State  ( State, evalState, get, put )

-------------------------------------------------------------------------------
-- Effect and Actions                                                        --
-------------------------------------------------------------------------------
-- | An effect capable of setting and getting the file path and the content of
--   the input file.
--
--   While setting the input file, the content is converted to lines of code.
data InputFile m a where
  SetFile :: FilePath -> String -> InputFile m ()
  GetFile :: InputFile m (FilePath, [String])

makeSem ''InputFile

-------------------------------------------------------------------------------
-- Interpretations                                                           --
-------------------------------------------------------------------------------
-- | Handles a computation by providing initially empty file data.
runInputFile :: Sem (InputFile ': r) a -> Sem r a
runInputFile = runWithInputFile ("", [])

-- | Handles a computation by providing the supplied file data to read from or
--   to set new file data.
runWithInputFile :: (FilePath, [String]) -> Sem (InputFile ': r) a -> Sem r a
runWithInputFile initialFileData = evalState initialFileData . inputFileToState
 where
  -- | Reinterprets 'InputFile' in terms of 'State'.
  inputFileToState :: Sem (InputFile ': r) a -> Sem (State (FilePath, [String]) ': r) a
  inputFileToState = reinterpret \case
    SetFile path content -> put (path, lines content)
    GetFile              -> get