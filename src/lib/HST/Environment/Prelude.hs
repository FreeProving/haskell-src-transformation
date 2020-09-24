-- | This module contains a module interface containing environment entries for
--   built-in data types and data constructors.
module HST.Environment.Prelude ( preludeModuleInterface ) where

import qualified Data.Map.Strict        as Map

import           HST.Effect.InputModule
  ( ConEntry(..), DataEntry(..), ModuleInterface(..), TypeName, createDataMapEntry , createConMapEntries )
import qualified HST.Frontend.Syntax    as S

-- | A module interface for built-in data types.
preludeModuleInterface :: ModuleInterface a
preludeModuleInterface = ModuleInterface
  { interfaceModName     = Just (S.ModuleName S.NoSrcSpan "Prelude")
  , interfaceDataEntries = Map.fromList
      (map createDataMapEntry preludeEnvironmentEntries)
  , interfaceConEntries  = Map.fromList
      (concatMap createConMapEntries preludeEnvironmentEntries)
  }

-- | Environment entries for built-in data types.
preludeEnvironmentEntries :: [DataEntry a]
preludeEnvironmentEntries = [ DataEntry unitTypeName [unitConEntry]
                            , DataEntry pairTypeName [pairConEntry]
                            , DataEntry listTypeName listConEntries
                            ]

-------------------------------------------------------------------------------
-- Unit                                                                      --
-------------------------------------------------------------------------------
-- | Environment entry for the unit data type name.
unitTypeName :: TypeName a
unitTypeName = S.Special S.NoSrcSpan (S.UnitCon S.NoSrcSpan)

-- | Environment entry for the unit data constructor.
unitConEntry :: ConEntry a
unitConEntry = ConEntry
  { conEntryName    = S.Special S.NoSrcSpan (S.UnitCon S.NoSrcSpan)
  , conEntryArity   = 0
  , conEntryIsInfix = False
  , conEntryType    = unitTypeName
  }

-------------------------------------------------------------------------------
-- Pairs                                                                     --
-------------------------------------------------------------------------------
-- | Environment entry for the pair data type name.
pairTypeName :: TypeName a
pairTypeName = S.Special S.NoSrcSpan (S.TupleCon S.NoSrcSpan S.Boxed 2)

-- | Environment entry for the pair data constructor.
pairConEntry :: ConEntry a
pairConEntry = ConEntry
  { conEntryName    = S.Special S.NoSrcSpan (S.TupleCon S.NoSrcSpan S.Boxed 2)
  , conEntryArity   = 2
  , conEntryIsInfix = False
  , conEntryType    = pairTypeName
  }

-------------------------------------------------------------------------------
-- Lists                                                                     --
-------------------------------------------------------------------------------
-- | Environment entry for the list data type name.
listTypeName :: TypeName a
listTypeName = S.Special S.NoSrcSpan (S.NilCon S.NoSrcSpan)

-- | Environment entries for the list data constructors.
listConEntries :: [ConEntry a]
listConEntries
  = [ ConEntry { conEntryName    = S.Special S.NoSrcSpan (S.NilCon S.NoSrcSpan)
               , conEntryArity   = 0
               , conEntryIsInfix = False
               , conEntryType    = listTypeName
               }
    , ConEntry { conEntryName    = S.Special S.NoSrcSpan (S.ConsCon S.NoSrcSpan)
               , conEntryArity   = 2
               , conEntryIsInfix = True
               , conEntryType    = listTypeName
               }
    ]
