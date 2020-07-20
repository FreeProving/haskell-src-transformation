-- | This module contains commonly used getter functions that report fatal
--   internal errors if an AST node does not match.

module HST.Util.Selectors where

import           Polysemy                       ( Member
                                                , Sem
                                                )

import           HST.Effect.Report              ( Message(..)
                                                , Report
                                                , Severity(Internal)
                                                , reportFatal
                                                )
import qualified HST.Frontend.Syntax           as S

-- | Gets the expressions of the given right-hand side of a rule.
fromUnguardedRhs :: Member Report r => S.Rhs a -> Sem r (S.Exp a)
fromUnguardedRhs (S.UnGuardedRhs _ expr) = return expr
fromUnguardedRhs (S.GuardedRhss _ _) =
  reportFatal $ Message Internal "Expected unguarded right-hand side."
