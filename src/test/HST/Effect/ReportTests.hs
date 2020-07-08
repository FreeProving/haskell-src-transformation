module HST.Effect.ReportTests ( testReportEffect ) where

import HST.Effect.Report ( Message, Report, runReport )

import           Polysemy                       ( Member
                                                , Members
                                                , Sem
                                                , run
                                                , runM
                                                )

import           Test.Hspec                     ( Expectation
                                                , Spec
                                                , context
                                                , describe
                                                , it
                                                , shouldBe
                                                , shouldReturn
                                                , shouldThrow
                                                )



testReportEffect :: Spec
testReportEffect = describe "HST.Effect.Report" $ do
  testRunReport

testRunReport :: Spec
testRunReport = context "runReport" $ do
  it "returns Just a value and no messages if nothing is reported" $ do
    let comp :: Member Report r => Sem r Int
        comp = return 42
    run (runReport comp) `shouldBe` ([], Just 42)
