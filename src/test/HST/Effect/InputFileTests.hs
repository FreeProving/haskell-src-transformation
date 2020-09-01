module HST.Effect.InputFileTests ( testInputFileEffect ) where

import Control.Exception ( finally )
import Polysemy (Member, Sem, runM)
import HST.Effect.InputFile ( InputFile, getInputFile, runInputFile )

import System.Directory
import System.IO
import System.IO.Error

import Test.Hspec (Spec, context, describe, it, shouldBe, shouldReturn)

testInputFileEffect :: Spec
testInputFileEffect = describe "HST.Effect.InputFile" testRunInputFile

testRunInputFile :: Spec
testRunInputFile = context "runInputFile" $ do
  it "returns Nothing if no input files are given" $ do
    let comp :: Member InputFile r => Sem r (Maybe String)
        comp = getInputFile "." >>= return
    runM (runInputFile [] comp) `shouldReturn` Nothing
--  it "returns the file content if the given file is in the list of filepaths" $ do
--    return ()
