
import           HST.Frontend.Transformer.Messages ( displayCodeExcerpt )
import           Test.Hspec
  ( Spec, context, describe, it, shouldBe, shouldReturn )


testMessages :: Spec
testMessages = describe "HST.Frontend.Transformer" $ do
  testDisplayCodeExcerpt

testDisplayCodeExcerpt :: Spec
testDisplayCodeExcerpt = context "displayCodeExcerpt" $ do
  return ()
