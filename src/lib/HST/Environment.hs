-- | This module contains an abstract data type for the pattern matching
--   compiler's state.

module HST.Environment
  ( -- * Environment Entries
    ConEntry(..)
  , DataEntry(..)
    -- * Environment
  , Environment
  , emptyEnv
    -- * Lookup
  , lookupConEntry
  , lookupDataEntry
  , lookupMatchedPat
    -- * Insertion
  , insertConEntry
  , insertDataEntry
  , pushMatchedPat
  , popMatchedPat
    -- * Backward Compatibility
  , envToConstrMap
  )
where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( mapMaybe )

import           HST.Environment.FreshVars      ( Constructor )
import qualified HST.Frontend.Syntax           as S

-------------------------------------------------------------------------------
-- Aliases for Names in the Environment                                      --
-------------------------------------------------------------------------------

-- | The name of a data type.
type TypeName a = S.QName a

-- | The name of a data constructor.
type ConName a = S.QName a

-- | The name of a variable.
type VarName a = S.QName a

-------------------------------------------------------------------------------
-- Environment Entries                                                       --
-------------------------------------------------------------------------------

-- | An entry of the 'Environment' for a data constructor that is in scope.
data ConEntry a = ConEntry
  { conEntryName    :: ConName a
    -- ^ The name of the constructor.
  , conEntryArity   :: Int
    -- ^ The number of fields of the constructor.
  , conEntryIsInfix :: Bool
    -- ^ Whether the constructor should be written in infix notation or not.
  , conEntryType    ::  TypeName a
    -- ^ The name of the data type that the constructor belongs to.
  }

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
  , envMatchedPats :: Map (VarName a) [S.Pat a]
    -- ^ Maps names of local variables to a stack of patterns they
    --   have been matched against.
  }

-- | An empty 'Environment'.
emptyEnv :: Environment a
emptyEnv = Environment { envConEntries  = Map.empty
                       , envDataEntries = Map.empty
                       , envMatchedPats = Map.empty
                       }

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

-- | Looks up the pattern a variable has been matched against.
lookupMatchedPat :: VarName a -> Environment a -> Maybe (S.Pat a)
lookupMatchedPat name = fmap head . Map.lookup name . envMatchedPats

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

-- | Adds a pattern to the stack of patterns the variable with the given
--   name has been matched against.
pushMatchedPat :: VarName a -> S.Pat a -> Environment a -> Environment a
pushMatchedPat varName pat env = env
  { envMatchedPats = Map.alter (maybePrepend pat) varName (envMatchedPats env)
  }
 where
  -- | Like @(:)@ but returns @Just@ a singleton list if the given tail is
  --   @Nothing@.
  maybePrepend :: a -> Maybe [a] -> Maybe [a]
  maybePrepend x Nothing   = Just [x]
  maybePrepend x (Just xs) = Just (x : xs)

-- | Removes a pattern from the stack of patterns the variable with the given
--   name has been matched against.
popMatchedPat :: VarName a -> Environment a -> Environment a
popMatchedPat varName env = env
  { envMatchedPats = Map.update maybeTail varName (envMatchedPats env)
  }
 where
  -- | Like @tail@ but returns @Nothing@ if the list is empty.
  maybeTail :: [a] -> Maybe [a]
  maybeTail []       = Nothing
  maybeTail (_ : xs) = Just xs
-------------------------------------------------------------------------------
-- Backward Compatibility                                                    --
-------------------------------------------------------------------------------

-- | Converts an environment to the old constructor map of the
--   'HST.Envionrment.FreshVars.PM' monad.
envToConstrMap :: Environment a -> [(String, [Constructor a])]
envToConstrMap env = map mkConstrMapEntry (Map.elems (envDataEntries env))
 where
    {- mkConstrMapEntry :: DataEntry a -> (String, [Constructor a])
    -}
  mkConstrMapEntry entry =
    ( fromQName (dataEntryName entry)
    , mapMaybe lookupConstructor (dataEntryCons entry)
    )

  {- lookupConstructor :: ConName a -> Maybe (Constructor a) -}
  lookupConstructor name = do
    entry <- lookupConEntry name env
    return (conEntryName entry, conEntryArity entry, conEntryIsInfix entry)

  fromQName :: S.QName a -> String
  fromQName (S.Qual _ modName name) =
    fromModName modName ++ "." ++ fromName name
  fromQName (S.UnQual  _ name) = fromName name
  fromQName (S.Special _ con ) = fromSpecialCon con

  fromModName :: S.ModuleName a -> String
  fromModName (S.ModuleName _ modName) = modName

  fromName :: S.Name a -> String
  fromName (S.Ident  _ str) = str
  fromName (S.Symbol _ str) = str

  fromSpecialCon :: S.SpecialCon a -> String
  fromSpecialCon (S.UnitCon _             ) = "()"
  fromSpecialCon (S.ListCon _             ) = "[]"
  fromSpecialCon (S.FunCon  _             ) = "(->)"
  fromSpecialCon (S.TupleCon _ S.Boxed   n) = "(" ++ replicate n ',' ++ ")"
  fromSpecialCon (S.TupleCon _ S.Unboxed n) = "(#" ++ replicate n ',' ++ "#)"
  fromSpecialCon (S.Cons             _    ) = "(:)"
  fromSpecialCon (S.UnboxedSingleCon _    ) = "(##)"
  fromSpecialCon (S.ExprHole         _    ) = "_"
