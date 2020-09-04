-- | This module contains commonly used getter functions that report fatal
--   internal errors if an AST node does not match.
module HST.Util.Selectors
  ( -- * Right-hand sides
    expFromUnguardedRhs
    -- * Pattern Names
  , getAltConName
  , getPatConName
  , getMaybePatConName
  , getPatVarName
  ) where

import           Polysemy            ( Member, Members, Sem, run )

import           HST.Effect.Fresh    ( Fresh, freshIdent, genericFreshPrefix )
import           HST.Effect.Report   ( Report, evalReport, reportFatal )
import qualified HST.Frontend.Syntax as S
import           HST.Util.Messages   ( Severity(Error, Internal), message )

-------------------------------------------------------------------------------
-- Right-hand sides                                                          --
-------------------------------------------------------------------------------
-- | Gets the expression of the given unguarded right-hand side of a rule.
--
--   Reports a fatal internal error if the given right-hand side has a guard.
expFromUnguardedRhs :: Member Report r => S.Rhs a -> Sem r (S.Exp a)
expFromUnguardedRhs (S.UnGuardedRhs _ expr) = return expr
expFromUnguardedRhs (S.GuardedRhss _ _)     = reportFatal
  $ message Internal S.NoSrcSpan
  $ "Expected unguarded right-hand side."

-------------------------------------------------------------------------------
-- Pattern Names                                                             --
-------------------------------------------------------------------------------
-- | Gets the name of the outermost constructor matched by the given @case@
--   expression alternative.
getAltConName :: Member Report r => S.Alt a -> Sem r (S.QName a)
getAltConName (S.Alt _ p _ _) = getPatConName p

-- | Gets the name of the outermost constructor matched by the given pattern.
--
--   Reports a fatal error if the given pattern is not a constructor pattern.
getPatConName :: Member Report r => S.Pat a -> Sem r (S.QName a)
getPatConName (S.PApp _ conName _)        = return conName
getPatConName (S.PInfixApp _ _ conName _) = return conName
-- Constructor patterns with special syntax.
getPatConName (S.PList _ pats)
  | null pats = return $ S.Special S.NoSrcSpan (S.NilCon S.NoSrcSpan)
  | otherwise = return $ S.Special S.NoSrcSpan (S.ConsCon S.NoSrcSpan)
getPatConName (S.PTuple _ boxed pats)     = return
  $ S.Special S.NoSrcSpan (S.TupleCon S.NoSrcSpan boxed (length pats))
-- Look into parentheses recursively.
getPatConName (S.PParen _ pat)            = getPatConName pat
-- All other patterns are not constructor patterns.
getPatConName (S.PVar _ _)                = reportFatal
  $ message Error S.NoSrcSpan
  $ "Expected constructor pattern, got variable pattern."
getPatConName (S.PWildCard _)             = reportFatal
  $ message Error S.NoSrcSpan
  $ "Expected constructor pattern, got wildcard pattern."

-- | Like 'getPatConName' but returns @Nothing@ if the given pattern is not
--   a variable pattern.
getMaybePatConName :: S.Pat a -> Maybe (S.QName a)
getMaybePatConName = run . evalReport . getPatConName

-- | Extracts the name of the variable bound by the given variable pattern.
--
--   Returns a fresh variable for wildcard patterns.
--   The given pattern must be a variable or wildcard pattern. Otherwise a
--   fatal internal error is reported.
getPatVarName :: Members '[Fresh, Report] r => S.Pat a -> Sem r String
getPatVarName (S.PVar _ pname) = return (getNameStr pname)
 where
  getNameStr (S.Ident _ str)  = str
  getNameStr (S.Symbol _ str) = str
getPatVarName (S.PWildCard _) = freshIdent genericFreshPrefix
-- Look into parentheses recursively.
getPatVarName (S.PParen _ pat) = getPatVarName pat
-- All other patterns are not variable patterns.
getPatVarName (S.PApp _ _ _) = reportFatal
  $ message Error S.NoSrcSpan
  $ "Expected variable or wildcard pattern, got constructor pattern."
getPatVarName (S.PInfixApp _ _ _ _) = reportFatal
  $ message Error S.NoSrcSpan
  $ "Expected variable or wildcard pattern, got infix constructor pattern."
getPatVarName (S.PTuple _ _ _) = reportFatal
  $ message Error S.NoSrcSpan
  $ "Expected variable or wildcard pattern, got tuple pattern."
getPatVarName (S.PList _ _) = reportFatal
  $ message Error S.NoSrcSpan
  $ "Expected variable or wildcard pattern, got list pattern."
