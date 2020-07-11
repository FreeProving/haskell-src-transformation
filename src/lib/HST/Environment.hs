-- | This module contains an abstract data type for the pattern matching
--   compiler's state.

module HST.Environment
  ( Environment
  , emptyEnv
  )
where

-- | A data type for the state of the pattern matching compiler.
data Environment = Environment

emptyEnv :: Environment
emptyEnv = Environment
