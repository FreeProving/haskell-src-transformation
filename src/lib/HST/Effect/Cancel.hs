{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}

-- | This module defines an effect for computations that can be canceled
--   prematurely without returning a value or throwing a particular error.
--
--   Possible interpretations of this effect include returning a @Maybe@
--   value or exiting the application in the @IO@ monad.

module HST.Effect.Cancel
  (
    -- * Effect
    Cancel
    -- * Actions
  , cancel
    -- * Interpretations
  , runCancel
  , cancelToExit
  )
where

import           Data.Either.Extra              ( eitherToMaybe )
import           Polysemy                       ( Member
                                                , Sem
                                                , interpret
                                                , makeSem
                                                , reinterpret
                                                )
import           Polysemy.Embed                 ( Embed
                                                , embed
                                                )
import           Polysemy.Error                 ( runError
                                                , throw
                                                )
import           System.Exit                    ( exitFailure )

-------------------------------------------------------------------------------
-- Effect and Actions                                                        --
-------------------------------------------------------------------------------

-- | An effect capable of canceling the computation prematurely.
data Cancel m a where
  Cancel ::Cancel m a

makeSem ''Cancel

-------------------------------------------------------------------------------
-- Interpretations                                                           --
-------------------------------------------------------------------------------

-- | Interprets a cancelable computation by wrapping its return value in
--   @Just@ and returning @Nothing@ if the computation is canceled.
runCancel :: Sem (Cancel ': r) a -> Sem r (Maybe a)
runCancel = fmap eitherToMaybe . runError . reinterpret \case
  Cancel -> throw ()

-- | Interprets a cancelable computation in the @IO@ monad by exiting from
--   the entire application with a non-zero status code (using 'exitFailure')
--   when the computation is canceled.
cancelToExit :: Member (Embed IO) r => Sem (Cancel ': r) a -> Sem r a
cancelToExit = interpret \case
  Cancel -> embed exitFailure
