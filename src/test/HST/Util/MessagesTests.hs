module HST.Util.MessagesTests ( testMessages ) where

import           Data.Map.Strict      ( Map )
import qualified Data.Map.Strict      as Map
import           Polysemy             ( run )
import           Test.Hspec           ( Spec, context, describe, it, shouldBe )

import           HST.Effect.InputFile ( runInputFileNoIO )
import qualified HST.Frontend.Syntax  as S
import           HST.Util.Messages    ( displayCodeExcerpt )

-- | Tests for the "HST.Util.Messages" module.
testMessages :: Spec
testMessages = describe "HST.Util.Messages" $ do
  testDisplayCodeExcerpt

-- | Test cases for 'displayCodeExcerpt'.
testDisplayCodeExcerpt :: Spec
testDisplayCodeExcerpt = context "displayCodeExcerpt" $ do
  it "should return the empty string if no source span is given"
    $ let str = run . runInputFileNoIO exampleMap $ displayCodeExcerpt Nothing
      in str `shouldBe` []
  it "should display an error message if end line < start line"
    $ let str = run . runInputFileNoIO exampleMap
            $ displayCodeExcerpt
            $ Just invalidSrcSpan2
      in str `shouldBe` "The source span 2:4-1:5 of `"
         ++ exampleFilePath
         ++ "` cannot be fully displayed!"
  it "should display an error message if end column < start column and start line = end line"
    $ let str = run . runInputFileNoIO exampleMap
            $ displayCodeExcerpt
            $ Just invalidSrcSpan
      in str `shouldBe` "The source span 1:4-1:3 of `"
         ++ exampleFilePath
         ++ "` cannot be fully displayed!"
  it "should correctly display a one-line excerpt"
    $ let str = run . runInputFileNoIO exampleMap
            $ displayCodeExcerpt
            $ Just oneLineSrcSpan
      in str
         `shouldBe` unlines [ exampleFilePath ++ ":1:8-1:21:"
                            , "1 | module ExampleQueue1 where"
                            , "           ^^^^^^^^^^^^^"
                            ]
  it "should correctly display a multi-line SrcSpan with start column < end column"
    $ let str = run . runInputFileNoIO exampleMap
            $ displayCodeExcerpt
            $ Just multiLineSrcSpan1
      in str
         `shouldBe` unlines
         [ exampleFilePath ++ ":5:1-6:11:"
         , "    vvvvvvvvvvvvvvvv"
         , "5 | empty :: Queue a"
         , "6 | empty = []"
         , "    ^^^^^^^^^^"
         ]
  it "should correctly display a multi-line SrcSpan with end column < start column"
    $ let str = run . runInputFileNoIO exampleMap
            $ displayCodeExcerpt
            $ Just multiLineSrcSpan2
      in str
         `shouldBe` unlines
         [ exampleFilePath ++ ":5:10-6:6:"
         , "             vvvvvvv"
         , "5 | empty :: Queue a"
         , "6 | empty = []"
         , "    ^^^^^"
         ]
  it "should display a shortened excerpt correctly"
    $ let str = run . runInputFileNoIO exampleMap
              $ displayCodeExcerpt
              $ Just longSrcSpan
      in str `shouldBe` unlines [ exampleFilePath ++ ":3:1-9:19:"
                                , "    vvvvvvvvvvvvvvvvvvvvvvvvvv"
                                , "3 | type Queue a = [a]"
                                , "4 | "
                                , ":"
                                , "8 | isEmpty :: Queue a -> Bool"
                                , "9 | isEmpty q = null q"
                                , "    ^^^^^^^^^^^^^^^^^^"]

-- | A 'S.SrcSpan' whose end column is smaller than its start column.
invalidSrcSpan :: S.MsgSrcSpan
invalidSrcSpan = S.MsgSrcSpan exampleFilePath 1 4 1 3

-- | A 'S.SrcSpan' that is not valid because the start line is below the end
--   line.
invalidSrcSpan2 :: S.MsgSrcSpan
invalidSrcSpan2 = S.MsgSrcSpan exampleFilePath 2 4 1 5

-- | A 'S.SrcSpan' that spans one line.
oneLineSrcSpan :: S.MsgSrcSpan
oneLineSrcSpan = S.MsgSrcSpan exampleFilePath 1 8 1 21

-- | A 'S.SrcSpan' spanning multiple lines.
multiLineSrcSpan1 :: S.MsgSrcSpan
multiLineSrcSpan1 = S.MsgSrcSpan exampleFilePath 5 1 6 11

-- | A 'S.SrcSpan' spanning multiple lines and whose end column is smaller than
--   the start column.
multiLineSrcSpan2 :: S.MsgSrcSpan
multiLineSrcSpan2 = S.MsgSrcSpan exampleFilePath 5 10 6 6

longSrcSpan :: S.MsgSrcSpan
longSrcSpan = S.MsgSrcSpan exampleFilePath 3 1 9 19

-- | The path to the file used for testing.
exampleFilePath :: FilePath
exampleFilePath = "./example/ExampleQueue1.hs"

-- | The content of the example file.
exampleContent :: String
exampleContent = unlines
  [ "module ExampleQueue1 where"
  , ""
  , "type Queue a = [a]"
  , ""
  , "empty :: Queue a"
  , "empty = []"
  , ""
  , "isEmpty :: Queue a -> Bool"
  , "isEmpty q = null q"
  , ""
  , "front :: Queue a -> a"
  , "front (x : q) = x"
  , ""
  , "add :: a -> Queue a -> Queue a"
  , "add x q = q ++ [x]"
  ]

-- | The map to use for the handler of `HST.Effect.InputFile`.
exampleMap :: Map FilePath String
exampleMap = Map.singleton exampleFilePath exampleContent
