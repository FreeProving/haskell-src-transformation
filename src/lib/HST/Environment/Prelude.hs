-- | This module contains environment entries for built-in data types and
--   data constructors.

module HST.Environment.Prelude
  ( insertPreludeEntries
  )
where

import           Polysemy                       ( Member
                                                , Sem
                                                )

import           HST.Effect.Env                 ( Env
                                                , modifyEnv
                                                )
import           HST.Environment                ( ConEntry(..)
                                                , DataEntry(..)
                                                , insertConEntry
                                                , insertDataEntry
                                                )
import qualified HST.Frontend.Syntax           as S

-- | Inserts entries for build-in data types and data constructors into the
--   environment.
insertPreludeEntries :: Member (Env a) r => Sem r ()
insertPreludeEntries = do
  mapM_ (modifyEnv . insertDataEntry) preludeDataEntries
  mapM_ (modifyEnv . insertConEntry)  preludeConEntries

-- | Environment entries for built-in data types.
preludeDataEntries :: [DataEntry a]
preludeDataEntries = [unitDataEntry, pairDataEntry, listDataEntry]

-- | Environment entries for built-in data constructors.
preludeConEntries :: [ConEntry a]
preludeConEntries = [unitConEntry, pairConEntry] ++ listConEntries

-------------------------------------------------------------------------------
-- Unit                                                                      --
-------------------------------------------------------------------------------

-- | Environment entry for the unit data type.
unitDataEntry :: DataEntry a
unitDataEntry = DataEntry
  { dataEntryName = S.Special S.NoSrcSpan (S.UnitCon S.NoSrcSpan)
  , dataEntryCons = [conEntryName unitConEntry]
  }

-- | Environment entry for the unit data constructor.
unitConEntry :: ConEntry a
unitConEntry = ConEntry
  { conEntryName    = S.Special S.NoSrcSpan (S.UnitCon S.NoSrcSpan)
  , conEntryArity   = 0
  , conEntryIsInfix = False
  , conEntryType    = dataEntryName unitDataEntry
  }

-------------------------------------------------------------------------------
-- Pairs                                                                     --
-------------------------------------------------------------------------------

-- | Environment entry for the pair data type.
pairDataEntry :: DataEntry a
pairDataEntry = DataEntry
  { dataEntryName = S.Special S.NoSrcSpan (S.TupleCon S.NoSrcSpan S.Boxed 2)
  , dataEntryCons = [conEntryName pairConEntry]
  }

-- | Environment entry for the pair data constructor.
pairConEntry :: ConEntry a
pairConEntry = ConEntry
  { conEntryName    = S.Special S.NoSrcSpan (S.TupleCon S.NoSrcSpan S.Boxed 2)
  , conEntryArity   = 2
  , conEntryIsInfix = False
  , conEntryType    = dataEntryName pairDataEntry
  }

-------------------------------------------------------------------------------
-- Lists                                                                     --
-------------------------------------------------------------------------------

-- | Environment entry for the list data type.
listDataEntry :: DataEntry a
listDataEntry = DataEntry
  { dataEntryName = S.Special S.NoSrcSpan (S.ListCon S.NoSrcSpan)
  , dataEntryCons = map conEntryName listConEntries
  }

-- | Environment entry for the pair data constructor.
listConEntries :: [ConEntry a]
listConEntries =
  [ ConEntry { conEntryName    = S.Special S.NoSrcSpan (S.ListCon S.NoSrcSpan)
             , conEntryArity   = 0
             , conEntryIsInfix = False
             , conEntryType    = dataEntryName listDataEntry
             }
  , ConEntry { conEntryName    = S.Special S.NoSrcSpan (S.Cons S.NoSrcSpan)
             , conEntryArity   = 2
             , conEntryIsInfix = True
             , conEntryType    = dataEntryName listDataEntry
             }
  ]
