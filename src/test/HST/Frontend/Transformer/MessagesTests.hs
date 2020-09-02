module HST.Frontend.Transformer.MessagesTests ( testMessages ) where

import           Polysemy             ( runM )
import           System.Directory     ( getCurrentDirectory )
import           Test.Hspec
  ( Spec, context, describe, it, shouldBe, shouldReturn )

import           HST.Effect.InputFile ( runInputFile )
import           HST.Effect.Report    ( displayCodeExcerpt )
import qualified HST.Frontend.Syntax  as S

testMessages :: Spec
testMessages = describe "HST.Frontend.Transformer" $ do
  testDisplayCodeExcerpt

testDisplayCodeExcerpt :: Spec
testDisplayCodeExcerpt = context "displayCodeExcerpt" $ do
  it "should return the empty string if no SrcSpan is given" $ do
    str <- runM . runInputFile [firstFile, secondFile]
      $ displayCodeExcerpt Nothing
    str `shouldBe` []
  it "should display an error message if the SrcSpan is invalid" $ do
    str <- runM . runInputFile [firstFile, secondFile]
      $ displayCodeExcerpt
      $ Just invalidSrcSpan
    str `shouldBe` "The source span 2:4-1:3 of `"
      ++ firstFile
      ++ "` cannot be fully displayed!"
  it "should correctly display a one-line SrcSpan" $ do
    str <- runM . runInputFile [firstFile, secondFile]
      $ displayCodeExcerpt
      $ Just oneLineSrcSpan
    str
      `shouldBe` unlines [ firstFile ++ ":1:8-1:21:"
                         , "1 | module ExampleQueue1 where"
                         , "           ^^^^^^^^^^^^^"
                         ]
  it "should correctly display a multi-line SrcSpan with startY < endY" $ do
    str <- runM . runInputFile [firstFile, secondFile]
      $ displayCodeExcerpt
      $ Just multiLineSrcSpan1
    str
      `shouldBe` unlines
      [ firstFile ++ ":5:1-6:11:"
      , "    vvvvvvvvvvvvvvvv"
      , "5 | empty :: Queue a"
      , "6 | empty = []"
      , "    ^^^^^^^^^^"
      ]

invalidSrcSpan :: S.MsgSrcSpan
invalidSrcSpan = S.MsgSrcSpan firstFile 2 4 1 3

oneLineSrcSpan :: S.MsgSrcSpan
oneLineSrcSpan = S.MsgSrcSpan firstFile 1 8 1 21

multiLineSrcSpan1 :: S.MsgSrcSpan
multiLineSrcSpan1 = S.MsgSrcSpan firstFile 5 1 6 11

firstFile :: FilePath
firstFile = "./example/ExampleQueue1.hs"

secondFile :: FilePath
secondFile = "./example/ExampleQueue2.hs"
