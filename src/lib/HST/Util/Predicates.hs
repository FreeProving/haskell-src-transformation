-- | This module contains commonly used predicate functions for AST nodes.

module HST.Util.Predicates
  ( -- * Pattern Predicates
    isConPat
  , isVarPat
  )
where

import qualified HST.Frontend.Syntax           as S

-- | Tests whether the given pattern is a constructor pattern.
--
--   Special patterns for lists and tuples are also considered constructor
--   patterns.
isConPat :: S.Pat a -> Bool
isConPat (S.PApp _ _ _       ) = True
isConPat (S.PInfixApp _ _ _ _) = True
isConPat (S.PList _ _        ) = True
isConPat (S.PTuple _ _ _     ) = True
isConPat (S.PParen _ p       ) = isConPat p

  -- All other patterns are variable patterns.
isConPat (S.PVar   _ _       ) = False
isConPat (S.PWildCard _      ) = False

-- | Tests whether the given pattern is a variable or wildcard pattern.
isVarPat :: S.Pat a -> Bool
isVarPat (S.PVar _ _         ) = True
isVarPat (S.PWildCard _      ) = True
isVarPat (S.PParen _ p       ) = isVarPat p

-- All other patterns are not variable patterns.
isVarPat (S.PApp _ _ _       ) = False
isVarPat (S.PInfixApp _ _ _ _) = False
isVarPat (S.PList _ _        ) = False
isVarPat (S.PTuple _ _ _     ) = False
