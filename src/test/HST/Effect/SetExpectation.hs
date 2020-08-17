{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module contains an effect for setting expectations in computations.
--
--   The 'SetExpectation' effect is basically just an alias for @'Embed' IO@
--   that has a more descriptive name.
module HST.Effect.SetExpectation
  ( -- * Effect
    SetExpectation
    -- * Actions
  , setExpectation
  , assertFailure
    -- * Interpretations
  , setExpectationToIO
    -- * Interpretations for Other Effects
  , reportToSetExpectation
  ) where

import           Polysemy ( Member, Sem, interpret, makeSem )
import           Polysemy.Embed ( Embed, embed )
import qualified Test.HUnit.Base as HUnit
import           Test.Hspec ( Expectation )

import           HST.Effect.Report ( Report, runReport, showPrettyMessage )

-------------------------------------------------------------------------------
-- Effect and Actions                                                        --
-------------------------------------------------------------------------------
-- | Effect cabable of setting 'Expectation's in computations.
data SetExpectation m a where
  SetExpectation :: Expectation -> SetExpectation m ()
  AssertFailure :: String -> SetExpectation m a

makeSem ''SetExpectation

-------------------------------------------------------------------------------
-- Interpretations                                                           --
-------------------------------------------------------------------------------
-- | Handles the 'SetExpectation' effect by embedding the expectations (which
--   are just @IO@ actions) into the computation.
setExpectationToIO
  :: Member (Embed IO) r => Sem (SetExpectation ': r) a -> Sem r a
setExpectationToIO = interpret \case
  SetExpectation expectation -> embed expectation
  AssertFailure msg          -> embed $ HUnit.assertFailure msg

-------------------------------------------------------------------------------
-- Interpretations for Other Effects                                         --
-------------------------------------------------------------------------------
-- | Handles the 'Report' effect by asserting that no fatal message is reported.
--
--   If there is a fatal message, all reported messages are included in
--   the error message.
reportToSetExpectation
  :: Member SetExpectation r => Sem (Report ': r) a -> Sem r a
reportToSetExpectation comp = do
  (ms, mx) <- runReport comp
  case mx of
    Nothing -> assertFailure
      $ unlines
      (("The following " ++ show (length ms) ++ " messages were reported:")
       : map showPrettyMessage ms)
    Just x  -> return x
