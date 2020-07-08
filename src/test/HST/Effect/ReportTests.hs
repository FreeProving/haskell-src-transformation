-- | This module contains tests for "HST.Effect.Cancel".

module HST.Effect.ReportTests
  ( testReportEffect
  )
where

import           HST.Effect.Cancel              ( runCancel )
import           HST.Effect.Report              ( Message(..)
                                                , Report
                                                , Severity(..)
                                                , report
                                                , reportFatal
                                                , reportToOutputOrCancel
                                                , runReport
                                                )

import           Polysemy                       ( Member
                                                , Sem
                                                , run
                                                )
import           Polysemy.Output                ( runOutputList )

import           Test.Hspec                     ( Spec
                                                , context
                                                , describe
                                                , it
                                                , shouldBe
                                                )


-- | Test group for interpreters of the 'HST.Effect.Report.Report' effect.
testReportEffect :: Spec
testReportEffect = describe "HST.Effect.Report" $ do
  testRunReport
  testReportToOutputOrCancel

-- | Test group for 'runReport' tests.
testRunReport :: Spec
testRunReport = context "runReport" $ do
  it "returns Just a value and no messages if nothing is reported" $ do
    let comp :: Member Report r => Sem r Int
        comp = return 42
    run (runReport comp) `shouldBe` ([], Just 42)
  it "returns Nothing and a single Message if a fatal error is reported" $ do
    let msg = Message Error "Some error"
        comp :: Member Report r => Sem r Int
        comp = reportFatal msg >> return 42
    run (runReport comp) `shouldBe` ([msg], Nothing)
  it "returns Just a value and a single message if a single message is reported"
    $ do
        let msg = Message Warning "Some warning"
            comp :: Member Report r => Sem r Int
            comp = report msg >> return 42
        run (runReport comp) `shouldBe` ([msg], Just 42)
  it "reports messages in the correct order" $ do
    let msg1 = Message Info "Some info"
        msg2 = Message Warning "Some warning"
        comp :: Member Report r => Sem r Int
        comp = report msg1 >> report msg2 >> return 42
    run (runReport comp) `shouldBe` ([msg1, msg2], Just 42)

-- | Test group for 'reportToOutputOrCancel' tests.
testReportToOutputOrCancel :: Spec
testReportToOutputOrCancel = context "reportToOutputOrCancel" $ do
  it "returns Just a value and no messages if nothing is reported" $ do
    let comp :: Member Report r => Sem r Int
        comp = return 42
    outputRun comp `shouldBe` ([], Just 42)
  it "returns Nothing and a single message if a fatal error is reported" $ do
    let msg = Message Error "Some error"
        comp :: Member Report r => Sem r Int
        comp = reportFatal msg >> return 42
    outputRun comp `shouldBe` ([msg], Nothing)
  it "returns Just a value and a single message if a single message is reported"
    $ do
        let msg = Message Warning "Some warning"
            comp :: Member Report r => Sem r Int
            comp = report msg >> return 42
        outputRun comp `shouldBe` ([msg], Just 42)
  it "reports messages in the correct order" $ do
    let msg1 = Message Info "Some info"
        msg2 = Message Warning "Some warning"
        comp :: Member Report r => Sem r Int
        comp = report msg1 >> report msg2 >> return 42
    outputRun comp `shouldBe` ([msg1, msg2], Just 42)
  where outputRun = run . runOutputList . runCancel . reportToOutputOrCancel
