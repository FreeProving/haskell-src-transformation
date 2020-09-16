-- | This module contains an abstract data type for the pattern matching
--   compiler's state.
module HST.Environment
  ( -- * Environment Entries
    DataEntry(..)
    -- * Environment
  , Environment
  , emptyEnv
    -- * Lookup
  , lookupConEntry
  , lookupDataEntry
    -- * Insertion
  , insertConEntry
  , insertDataEntry
  ) where

import           Data.Map.Strict     ( Map )
import qualified Data.Map.Strict     as Map

import           HST.Effect.InputModule ( ConEntry(conEntryName), ConName, TypeName )

-- | An entry of the 'Environment' for a data type whose constructors are in
--   scope.
data DataEntry a = DataEntry
  { dataEntryName :: TypeName a
    -- ^ The name of the data type.
  , dataEntryCons :: [ConName a]
    -- ^ The names of the data type's constructors.
  }

-------------------------------------------------------------------------------
-- Environment                                                               --
-------------------------------------------------------------------------------
-- | A data type for the state of the pattern matching compiler.
data Environment a = Environment
  { envConEntries  :: Map (ConName a) (ConEntry a)
    -- ^ Maps names of constructors to their 'ConEntry's.
  , envDataEntries :: Map (TypeName a) (DataEntry a)
    -- ^ Maps names of data types to their 'DataEntry's.
  }

-- | An empty 'Environment'.
emptyEnv :: Environment a
emptyEnv
  = Environment { envConEntries = Map.empty, envDataEntries = Map.empty }

-------------------------------------------------------------------------------
-- Lookup                                                                    --
-------------------------------------------------------------------------------
-- | Looks up the entry of a data constructor with the given name in the
--   environment.
--
--   Returns @Nothing@ if the data constructor is not in scope.
lookupConEntry :: ConName a -> Environment a -> Maybe (ConEntry a)
lookupConEntry name = Map.lookup name . envConEntries

-- | Looks up the entry of a data type with the given name in the environment.
--
--   Returns @Nothing@ if the data type is not in scope.
lookupDataEntry :: TypeName a -> Environment a -> Maybe (DataEntry a)
lookupDataEntry name = Map.lookup name . envDataEntries

-------------------------------------------------------------------------------
-- Insertion                                                                 --
-------------------------------------------------------------------------------
-- | Inserts the given entry for a data constructor into the environment.
insertConEntry :: ConEntry a -> Environment a -> Environment a
insertConEntry entry env = env
  { envConEntries = Map.insert (conEntryName entry) entry (envConEntries env)
  }

-- | Inserts the given entry for a data type into the environment.
insertDataEntry :: DataEntry a -> Environment a -> Environment a
insertDataEntry entry env = env
  { envDataEntries = Map.insert (dataEntryName entry) entry (envDataEntries env)
  }
