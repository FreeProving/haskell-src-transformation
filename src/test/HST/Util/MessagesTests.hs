module HST.Util.MessagesTests ( testMessages ) where

import           Polysemy             ( runM )
import           Test.Hspec           ( Spec, context, describe, it, shouldBe )

import           HST.Effect.InputFile ( runInputFile )
import qualified HST.Frontend.Syntax  as S
import           HST.Util.Messages    ( displayCodeExcerpt )

-- | Tests for the `HST.Util.Messages` module.
testMessages :: Spec
testMessages = describe "HST.Frontend.Transformer" $ do
  testDisplayCodeExcerpt

-- Test cases for `displayCodeExcerpt`.
testDisplayCodeExcerpt :: Spec
testDisplayCodeExcerpt = context "displayCodeExcerpt" $ do
  it "should return the empty string if no SrcSpan is given" $ do
    str <- runM . runInputFile $ displayCodeExcerpt Nothing
    str `shouldBe` []
  it "should display an error message if end column < start column" $ do
    str <- runM . runInputFile $ displayCodeExcerpt $ Just invalidSrcSpan
    str `shouldBe` "The source span 1:4-1:3 of `"
      ++ firstFile
      ++ "` cannot be fully displayed!"
  it "should display an error message if end line < start line" $ do
    str <- runM . runInputFile $ displayCodeExcerpt $ Just invalidSrcSpan2
    str `shouldBe` "The source span 2:4-1:5 of `"
      ++ firstFile
      ++ "` cannot be fully displayed!"
  it "should correctly display a one-line SrcSpan" $ do
    str <- runM . runInputFile $ displayCodeExcerpt $ Just oneLineSrcSpan
    str
      `shouldBe` unlines [ firstFile ++ ":1:8-1:21:"
                         , "1 | module ExampleQueue1 where"
                         , "           ^^^^^^^^^^^^^"
                         ]
  it "should correctly display a multi-line SrcSpan with startY < endY" $ do
    str <- runM . runInputFile $ displayCodeExcerpt $ Just multiLineSrcSpan1
    str
      `shouldBe` unlines
      [ firstFile ++ ":5:1-6:11:"
      , "    vvvvvvvvvvvvvvvv"
      , "5 | empty :: Queue a"
      , "6 | empty = []"
      , "    ^^^^^^^^^^"
      ]

-- | A `S.SrcSpan` which end column is smaller than its start column.
invalidSrcSpan :: S.MsgSrcSpan
invalidSrcSpan = S.MsgSrcSpan firstFile 1 4 1 3

-- | A `S.SrcSpan` that is not valid because the start line is below the end
--   line.
invalidSrcSpan2 :: S.MsgSrcSpan
invalidSrcSpan2 = S.MsgSrcSpan firstFile 2 4 1 5

-- | A `S.SrcSpan` that shows one line.
oneLineSrcSpan :: S.MsgSrcSpan
oneLineSrcSpan = S.MsgSrcSpan firstFile 1 8 1 21

-- | A `S.SrcSpan` over multiple lines.
multiLineSrcSpan1 :: S.MsgSrcSpan
multiLineSrcSpan1 = S.MsgSrcSpan firstFile 5 1 6 11

-- | The path to the file used for testing.
firstFile :: FilePath
firstFile = "./example/ExampleQueue1.hs"
--secondFile :: FilePath
--secondFile = "./example/ExampleQueue2.hs"
