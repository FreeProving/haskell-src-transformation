-- | This module contains commonly used getter functions that report fatal
--   internal errors if an AST node does not match.
module HST.Util.Selectors
  ( -- * Right-hand sides
    expFromUnguardedRhs
    -- * Pattern Names
  , getAltConName
  , getIdentifiers
  , getPatConName
  , getMaybePatConName
  , getPatVarName
  ) where

import           Data.Maybe          ( maybe )
import           Data.Set            ( Set )
import qualified Data.Set            as Set
import           Polysemy            ( Member, Members, Sem, run )

import           HST.Effect.Fresh    ( Fresh, freshIdent, genericFreshPrefix )
import           HST.Effect.Report
  ( Message(..), Report, Severity(Error, Internal), evalReport, reportFatal )
import qualified HST.Frontend.Syntax as S
import           HST.Util.PrettyName

-------------------------------------------------------------------------------
-- Right-hand sides                                                          --
-------------------------------------------------------------------------------
-- | Gets the expression of the given unguarded right-hand side of a rule.
--
--   Reports a fatal internal error if the given right-hand side has a guard.
expFromUnguardedRhs :: Member Report r => S.Rhs a -> Sem r (S.Exp a)
expFromUnguardedRhs (S.UnGuardedRhs _ expr) = return expr
expFromUnguardedRhs (S.GuardedRhss _ _)
  = reportFatal $ Message Internal $ "Expected unguarded right-hand side."

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
  $ Message Error
  $ "Expected constructor pattern, got variable pattern."
getPatConName (S.PWildCard _)             = reportFatal
  $ Message Error
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
  $ Message Error
  $ "Expected variable or wildcard pattern, got constructor pattern."
getPatVarName (S.PInfixApp _ _ _ _) = reportFatal
  $ Message Error
  $ "Expected variable or wildcard pattern, got infix constructor pattern."
getPatVarName (S.PTuple _ _ _) = reportFatal
  $ Message Error
  $ "Expected variable or wildcard pattern, got tuple pattern."
getPatVarName (S.PList _ _) = reportFatal
  $ Message Error
  $ "Expected variable or wildcard pattern, got list pattern."

-------------------------------------------------------------------------------
-- Find Identifiers in Modules                                               --
-------------------------------------------------------------------------------
-- | Collects all identifiers in a module in a set.
getIdentifiers :: S.Module a -> Set String
getIdentifiers (S.Module _ _ _ decls) = Set.unions
  (map getIdentifiersDecl decls)

-- | Collects all identifiers in a declaration.
getIdentifiersDecl :: S.Decl a -> Set String
getIdentifiersDecl (S.DataDecl _ _ _ _) = Set.empty
getIdentifiersDecl (S.FunBind _ ms)     = Set.unions
  (map getIdentifiersMatch ms)
getIdentifiersDecl (S.OtherDecl _ _)    = Set.empty

-- | Collects all identifiers in a pattern matching rule.
getIdentifiersMatch :: S.Match a -> Set String
getIdentifiersMatch (S.Match _ name pats rhs binds)          = Set.unions
  [ Set.singleton (prettyName name)
  , Set.unions (map getIdentifiersPat pats)
  , getIdentifiersRhs rhs
  , maybe Set.empty getIdentifiersBinds binds
  ]
getIdentifiersMatch (S.InfixMatch _ pat name pats rhs binds) = Set.unions
  [ Set.singleton (prettyName name)
  , Set.unions (map getIdentifiersPat (pat : pats))
  , getIdentifiersRhs rhs
  , maybe Set.empty getIdentifiersBinds binds
  ]

-- | Collects all identifiers in a pattern.
getIdentifiersPat :: S.Pat a -> Set String
getIdentifiersPat (S.PVar _ name)                 = Set.singleton
  (prettyName name)
getIdentifiersPat (S.PInfixApp _ pat1 qname pat2) = Set.insert
  (prettyName qname)
  (Set.union (getIdentifiersPat pat1) (getIdentifiersPat pat2))
getIdentifiersPat (S.PApp _ qname pats)           = Set.insert
  (prettyName qname) (Set.unions (map getIdentifiersPat pats))
getIdentifiersPat (S.PTuple _ _ pats)             = Set.unions
  (map getIdentifiersPat pats)
getIdentifiersPat (S.PParen _ pat)                = getIdentifiersPat pat
getIdentifiersPat (S.PList _ pats)                = Set.unions
  (map getIdentifiersPat pats)
getIdentifiersPat (S.PWildCard _)                 = Set.empty

-- | Collects all identifiers in a right-hand side of a pattern matching rule.
getIdentifiersRhs :: S.Rhs a -> Set String
getIdentifiersRhs (S.UnGuardedRhs _ expr) = getIdentifiersExp expr
getIdentifiersRhs (S.GuardedRhss _ grhss) = Set.unions
  (map getIdentifiersGRhs grhss)

-- | Collects all identifiers in a guarded right-hand side.
getIdentifiersGRhs :: S.GuardedRhs a -> Set String
getIdentifiersGRhs (S.GuardedRhs _ cond expr) = Set.union
  (getIdentifiersExp cond) (getIdentifiersExp expr)

-- | Collects all identifiers in an expression.
getIdentifiersExp :: S.Exp a -> Set String
getIdentifiersExp (S.Var _ qname)            = Set.singleton (prettyName qname)
getIdentifiersExp (S.Con _ qname)            = Set.singleton (prettyName qname)
getIdentifiersExp (S.Lit _ _)                = Set.empty
getIdentifiersExp (S.InfixApp _ exp1 _ exp2) = Set.union
  (getIdentifiersExp exp1) (getIdentifiersExp exp2)
getIdentifiersExp (S.App _ exp1 exp2)        = Set.union
  (getIdentifiersExp exp1) (getIdentifiersExp exp2)
getIdentifiersExp (S.NegApp _ expr)          = getIdentifiersExp expr
getIdentifiersExp (S.Lambda _ pats expr)     = Set.union
  (Set.unions (map getIdentifiersPat pats)) (getIdentifiersExp expr)
getIdentifiersExp (S.Let _ binds expr)       = Set.union
  (getIdentifiersBinds binds) (getIdentifiersExp expr)
getIdentifiersExp (S.If _ cond exp1 exp2)    = Set.union
  (getIdentifiersExp cond)
  (Set.union (getIdentifiersExp exp1) (getIdentifiersExp exp2))
getIdentifiersExp (S.Case _ scrutinee alts)  = Set.union
  (getIdentifiersExp scrutinee) (Set.unions (map getIdentifiersAlt alts))
getIdentifiersExp (S.Tuple _ _ exps)         = Set.unions
  (map getIdentifiersExp exps)
getIdentifiersExp (S.List _ exps)            = Set.unions
  (map getIdentifiersExp exps)
getIdentifiersExp (S.Paren _ expr)           = getIdentifiersExp expr
getIdentifiersExp (S.ExpTypeSig _ expr _)    = getIdentifiersExp expr

-- | Collects all identifiers in a @case@ alternative.
getIdentifiersAlt :: S.Alt a -> Set String
getIdentifiersAlt (S.Alt _ pat rhs binds) = Set.union (getIdentifiersPat pat)
  (Set.union (getIdentifiersRhs rhs) (maybe Set.empty getIdentifiersBinds binds))

-- | Collects all identifiers in @let@ and @where@ bindings.
getIdentifiersBinds :: S.Binds a -> Set String
getIdentifiersBinds (S.BDecls _ decls) = Set.unions
  (map getIdentifiersDecl decls)
