{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module HST.Effect.InputSrc where

import           Polysemy        ( Sem, makeSem, reinterpret )
import           Polysemy.State  ( State, evalState, get, put )

data InputSrc m a where
  SetSrc :: FilePath -> String -> InputSrc m ()
  GetPath :: InputSrc m FilePath
  GetLines :: InputSrc m [String]

makeSem ''InputSrc

type SrcFile = (FilePath, [String])

emptySrc :: SrcFile
emptySrc = ("", [])

runInputSrc :: Sem (InputSrc ': r) a -> Sem r a
runInputSrc = runWithInputSrc emptySrc

runWithInputSrc :: SrcFile -> Sem (InputSrc ': r) a -> Sem r a
runWithInputSrc initialSrc = evalState initialSrc . inputSrcToState
 where
  inputSrcToState :: Sem (InputSrc ': r) a -> Sem (State SrcFile ': r) a
  inputSrcToState = reinterpret \case
    SetSrc path content -> put (path, lines content)
    GetPath  -> fst <$> get
    GetLines -> snd <$> get