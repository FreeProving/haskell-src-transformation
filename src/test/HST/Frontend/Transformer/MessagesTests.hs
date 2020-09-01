module HST.Frontend.Transformer.MessagesTests ( testMessages ) where

import System.Directory ( getCurrentDirectory )
import           HST.Effect.InputFile ( runInputFile )
import           HST.Effect.Report ( displayCodeExcerpt )
import qualified HST.Frontend.Syntax as S
import           Polysemy ( runM )
import           Test.Hspec
  ( Spec, context, describe, it, shouldBe, shouldReturn )


testMessages :: Spec
testMessages = describe "HST.Frontend.Transformer" $ do
  testDisplayCodeExcerpt

testDisplayCodeExcerpt :: Spec
testDisplayCodeExcerpt = context "displayCodeExcerpt" $ do
  it "should return the empty string if no SrcSpan is given" $ do
    str <- runM . runInputFile [firstFile, secondFile] $ displayCodeExcerpt Nothing
    str `shouldBe` []
  it "should display an error message if the SrcSpan is invalid" $ do
    str <- runM . runInputFile [firstFile, secondFile] $ displayCodeExcerpt $ Just invalidSrcSpan
    str `shouldBe` "The source span 2:4-1:3 of `" ++ firstFile ++ "` cannot be fully displayed!"
  it "should correctly display a one-line SrcSpan" $ do
    str <- runM . runInputFile [firstFile, secondFile] $ displayCodeExcerpt $ Just oneLineSrcSpan
    str `shouldBe` unlines [firstFile ++ ":1:8-1:20:", "1 | module ExampleQueue1 where", "           ^^^^^^^^^^^^"]

invalidSrcSpan :: S.MsgSrcSpan
invalidSrcSpan = S.MsgSrcSpan firstFile 2 4 1 3

oneLineSrcSpan :: S.MsgSrcSpan
oneLineSrcSpan = S.MsgSrcSpan firstFile 1 8 1 20

firstFile :: FilePath
firstFile = "./example/ExampleQueue1.hs"

secondFile :: FilePath
secondFile = "./example/ExampleQueue2.hs"
