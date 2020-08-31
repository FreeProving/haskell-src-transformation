-- | This module contains tests for "HST.Effect.Report".
module HST.Effect.ReportTests ( testReportEffect ) where

import           Control.Exception    ( finally )
import           Polysemy             ( Member, Sem, run, runM )
import           Polysemy.Output      ( runOutputList )
import           System.Directory     ( getTemporaryDirectory, removeFile )
import           System.IO
import           System.IO.Error      ( catchIOError )
import           Test.Hspec
  ( Spec, context, describe, it, shouldBe, shouldReturn )

import           HST.Effect.Cancel    ( runCancel )
import           HST.Effect.InputFile ( runInputFile )
import           HST.Effect.Report
  ( Message(..), Report, Severity(..), filterReportedMessages, message, report
  , reportFatal, reportToHandleOrCancel, reportToOutputOrCancel, runReport )
import qualified HST.Frontend.Syntax  as S

-- | Test group for interpreters of the 'HST.Effect.Report.Report' effect.
testReportEffect :: Spec
testReportEffect = describe "HST.Effect.Report" $ do
  testRunReport
  testReportToOutputOrCancel
  testReportToHandleOrCancel
  testFilterReportedMessages

-- | Test group for 'runReport' tests.
testRunReport :: Spec
testRunReport = context "runReport" $ do
  it "returns Just a value and no messages if nothing is reported" $ do
    let comp :: Member Report r => Sem r Int
        comp = return 42
    run (runReport comp) `shouldBe` ([], Just 42)
  it "returns Nothing and a single Message if a fatal error is reported" $ do
    let msg = Message Error Nothing "Some error"
        comp :: Member Report r => Sem r Int
        comp = reportFatal msg >> return 42
    run (runReport comp) `shouldBe` ([msg], Nothing)
  it "returns Just a value and a single message if a single message is reported"
    $ do
      let msg = Message Warning Nothing "Some warning"
          comp :: Member Report r => Sem r Int
          comp = report msg >> return 42
      run (runReport comp) `shouldBe` ([msg], Just 42)
  it "reports messages in the correct order" $ do
    let msg1 = Message Info Nothing "Some info"
        msg2 = Message Warning Nothing "Some warning"
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
    let msg = Message Error Nothing "Some error"
        comp :: Member Report r => Sem r Int
        comp = reportFatal msg >> return 42
    outputRun comp `shouldBe` ([msg], Nothing)
  it "returns Just a value and a single message if a single message is reported"
    $ do
      let msg = Message Warning Nothing "Some warning"
          comp :: Member Report r => Sem r Int
          comp = report msg >> return 42
      outputRun comp `shouldBe` ([msg], Just 42)
  it "reports messages in the correct order" $ do
    let msg1 = Message Info Nothing "Some info"
        msg2 = Message Warning Nothing "Some warning"
        comp :: Member Report r => Sem r Int
        comp = report msg1 >> report msg2 >> return 42
    outputRun comp `shouldBe` ([msg1, msg2], Just 42)
 where
  outputRun = run . runOutputList . runCancel . reportToOutputOrCancel

-- | Test group for 'reportToHandleOrCancel' tests.
testReportToHandleOrCancel :: Spec
testReportToHandleOrCancel = context "reportToHandleOrCancel" $ do
  it "should write no message to the handle if nothing is reported"
    $ processCompHandle "tempFile"
    $ \h -> do
      let comp :: Member Report r => Sem r Int
          comp = return 42
      (runM . runCancel . runInputFile [] . reportToHandleOrCancel h) comp
        `shouldReturn` Just 42
      hSeek h AbsoluteSeek 0
      c <- hGetContents h
      null c `shouldBe` True
  it ("should return Nothing and write a single message to the handle "
      ++ "if a fatal message was reported")
    $ do
      processCompHandle "tempFile" $ \h -> do
        let msg = message Error S.NoSrcSpan "Some Error"
            comp :: Member Report r => Sem r Int
            comp = reportFatal msg >> return 42
        val <- (runM . runCancel . runInputFile [] . reportToHandleOrCancel h)
          comp
        hSeek h AbsoluteSeek 0
        c <- hGetContents h
        val `shouldBe` Nothing
        lines c `shouldBe` ["Error: Some Error"]
  it ("should return Just a value and write a single message to the handle "
      ++ "if a single message was reported")
    $ do
      processCompHandle "tempFile" $ \h -> do
        let msg = message Warning S.NoSrcSpan "Some Warning"
            comp :: Member Report r => Sem r Int
            comp = report msg >> return 42
        val <- (runM . runCancel . runInputFile [] . reportToHandleOrCancel h)
          comp
        hSeek h AbsoluteSeek 0
        c <- hGetContents h
        val `shouldBe` Just 42
        lines c `shouldBe` ["Warning: Some Warning"]
  it "should write reported messages to the handle in the right order"
    $ processCompHandle "tempFile"
    $ \h -> do
      let msg1 = message Info S.NoSrcSpan "Some Info"
      let msg2 = message Warning S.NoSrcSpan "Some Warning"
          comp :: Member Report r => Sem r Int
          comp = report msg1 >> report msg2 >> return 42
      val <- (runM . runCancel . runInputFile [] . reportToHandleOrCancel h)
        comp
      hSeek h AbsoluteSeek 0
      c <- hGetContents h
      val `shouldBe` Just 42
      lines c `shouldBe` ["Info: Some Info", "Warning: Some Warning"]

-- | Test group for 'filterReportedMessages' tests.
testFilterReportedMessages :: Spec
testFilterReportedMessages = context "filterReportedMessages" $ do
  it "should report nothing if predicate is always false" $ do
    let msg1 = Message Info Nothing "Some info"
        comp :: Member Report r => Sem r Int
        comp = report msg1 >> return 42
    runFilter (const False) comp `shouldBe` ([], Just 42)
  it "should report no messages for tautologic predicate and no reports" $ do
    let comp :: Member Report r => Sem r Int
        comp = return 42
    runFilter (const True) comp `shouldBe` ([], Just 42)
  it "should report all messages for tautologic predicate" $ do
    let msg = Message Warning Nothing "Some warning"
        comp, comp2 :: Member Report r => Sem r Int
        comp = report msg >> return 42
        comp2 = reportFatal msg >> return 42
    runFilter (const True) comp `shouldBe` ([msg], Just 42)
    runFilter (const True) comp2 `shouldBe` ([msg], Nothing)
  it "should report all messages if all reported messages satisfy the predicate"
    $ do
      let msg1 = Message Warning Nothing "Some warning"
          msg2 = Message Warning Nothing "Another warning"
          comp :: Member Report r => Sem r Int
          comp = report msg1 >> report msg2 >> return 42
      runFilter isWarn comp `shouldBe` ([msg1, msg2], Just 42)
  it "should report no messages if no reported message satisfies the predicate"
    $ do
      let msg1 = Message Info Nothing "Some info"
          msg2 = Message Error Nothing "Some error"
          comp :: Member Report r => Sem r Int
          comp = report msg1 >> report msg2 >> return 42
      runFilter isWarn comp `shouldBe` ([], Just 42)
  it "should report all messages that satisfy the predicate" $ do
    let msg1 = Message Info Nothing "Some info"
        msg2 = Message Warning Nothing "Some warning"
        comp :: Member Report r => Sem r Int
        comp = report msg1 >> report msg2 >> return 42
    runFilter isWarn comp `shouldBe` ([msg2], Just 42)
 where
  runFilter p = run . runReport . filterReportedMessages p

  isWarn      = (== Warning) . msgSeverity

-- | Opens a temporary file, then processes it and deletes it afterwards.
processCompHandle :: String -> (Handle -> IO a) -> IO a
processCompHandle pat comp = do
  tempdir <- catchIOError getTemporaryDirectory (\_ -> return ".")
  (tempfile, temph) <- openTempFile tempdir pat
  finally (comp temph) (do
                          hClose temph
                          removeFile tempfile)
