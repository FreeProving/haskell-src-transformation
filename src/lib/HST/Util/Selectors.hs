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
    -- * Identifiers
  , findIdentifiers
  ) where

import           Data.Maybe          ( maybe )
import           Data.Set            ( Set )
import qualified Data.Set            as Set
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
expFromUnguardedRhs (S.GuardedRhss s _)
  = reportFatal $ message Internal s $ "Expected unguarded right-hand side."

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
getPatConName (S.PList s pats)
  | null pats = return $ S.Special s (S.NilCon s)
  | otherwise = return $ S.Special s (S.ConsCon s)
getPatConName (S.PTuple s boxed pats)     = return
  $ S.Special s (S.TupleCon s boxed (length pats))
-- Look into parentheses recursively.
getPatConName (S.PParen _ pat)            = getPatConName pat
-- All other patterns are not constructor patterns.
getPatConName (S.PVar s _)                = reportFatal
  $ message Error s
  $ "Expected constructor pattern, got variable pattern."
getPatConName (S.PWildCard s)             = reportFatal
  $ message Error s
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
getPatVarName :: Members '[Fresh, Report] r => S.Pat a -> Sem r (S.Name a)
getPatVarName (S.PVar _ varName)    = return varName
getPatVarName (S.PWildCard srcSpan)
  = S.Ident srcSpan <$> freshIdent genericFreshPrefix
-- Look into parentheses recursively.
getPatVarName (S.PParen _ pat)      = getPatVarName pat
-- All other patterns are not variable patterns.
getPatVarName (S.PApp s _ _)        = reportFatal
  $ message Error s
  $ "Expected variable or wildcard pattern, got constructor pattern."
getPatVarName (S.PInfixApp s _ _ _) = reportFatal
  $ message Error s
  $ "Expected variable or wildcard pattern, got infix constructor pattern."
getPatVarName (S.PTuple s _ _)      = reportFatal
  $ message Error s
  $ "Expected variable or wildcard pattern, got tuple pattern."
getPatVarName (S.PList s _)         = reportFatal
  $ message Error s
  $ "Expected variable or wildcard pattern, got list pattern."

-------------------------------------------------------------------------------
-- Find Identifiers in Modules                                               --
-------------------------------------------------------------------------------
-- | Class for all types that contain identifiers.
class HasIdentifiers a where
  findIdentifiers :: a -> Set String

-- | Collects all identifiers in a list of types containing identifiers.
instance HasIdentifiers a => HasIdentifiers [a] where
  findIdentifiers = Set.unions . map findIdentifiers

-- | Collects all identifiers in a `Maybe` value.
instance HasIdentifiers a => HasIdentifiers (Maybe a) where
  findIdentifiers = maybe Set.empty findIdentifiers

-- | Collects all identifiers in a module in a set.
instance HasIdentifiers (S.Module a) where
  findIdentifiers (S.Module _ _ _ _ decls) = findIdentifiers decls

-- | Collects all identifiers in a declaration.
instance HasIdentifiers (S.Decl a) where
  findIdentifiers (S.DataDecl _ _ _ _) = Set.empty
  findIdentifiers (S.FunBind _ ms)     = findIdentifiers ms
  findIdentifiers (S.OtherDecl _ _)    = Set.empty

-- | Collects all identifiers in a pattern matching rule.
instance HasIdentifiers (S.Match a) where
  findIdentifiers (S.Match _ _ name pats rhs binds) = Set.unions
    [ findIdentifiers name
    , findIdentifiers pats
    , findIdentifiers rhs
    , findIdentifiers binds
    ]

-- | Collects all identifiers in a pattern.
instance HasIdentifiers (S.Pat a) where
  findIdentifiers (S.PVar _ name)                 = findIdentifiers name
  findIdentifiers (S.PInfixApp _ pat1 qname pat2) = findIdentifiers qname
    `Set.union` (findIdentifiers pat1 `Set.union` findIdentifiers pat2)
  findIdentifiers (S.PApp _ qname pats)
    = findIdentifiers qname `Set.union` findIdentifiers pats
  findIdentifiers (S.PTuple _ _ pats)             = findIdentifiers pats
  findIdentifiers (S.PParen _ pat)                = findIdentifiers pat
  findIdentifiers (S.PList _ pats)                = findIdentifiers pats
  findIdentifiers (S.PWildCard _)                 = Set.empty

-- | Collects all identifiers in a right-hand side of a pattern matching rule.
instance HasIdentifiers (S.Rhs a) where
  findIdentifiers (S.UnGuardedRhs _ expr) = findIdentifiers expr
  findIdentifiers (S.GuardedRhss _ grhss) = findIdentifiers grhss

-- | Collects all identifiers in a guarded right-hand side.
instance HasIdentifiers (S.GuardedRhs a) where
  findIdentifiers (S.GuardedRhs _ cond expr)
    = findIdentifiers cond `Set.union` findIdentifiers expr

-- | Collects all identifiers in an expression.
instance HasIdentifiers (S.Exp a) where
  findIdentifiers (S.Var _ qname)            = findIdentifiers qname
  findIdentifiers (S.Con _ qname)            = findIdentifiers qname
  findIdentifiers (S.Lit _ _)                = Set.empty
  findIdentifiers (S.InfixApp _ exp1 _ exp2)
    = findIdentifiers exp1 `Set.union` findIdentifiers exp2
  findIdentifiers (S.App _ exp1 exp2)
    = findIdentifiers exp1 `Set.union` findIdentifiers exp2
  findIdentifiers (S.NegApp _ expr)          = findIdentifiers expr
  findIdentifiers (S.Lambda _ pats expr)
    = findIdentifiers pats `Set.union` findIdentifiers expr
  findIdentifiers (S.Let _ binds expr)
    = findIdentifiers binds `Set.union` findIdentifiers expr
  findIdentifiers (S.If _ cond exp1 exp2)    = findIdentifiers cond
    `Set.union` findIdentifiers exp1
    `Set.union` findIdentifiers exp2
  findIdentifiers (S.Case _ scrutinee alts)
    = findIdentifiers scrutinee `Set.union` findIdentifiers alts
  findIdentifiers (S.Tuple _ _ exps)         = findIdentifiers exps
  findIdentifiers (S.List _ exps)            = findIdentifiers exps
  findIdentifiers (S.Paren _ expr)           = findIdentifiers expr
  findIdentifiers (S.ExpTypeSig _ expr _)    = findIdentifiers expr

-- | Collects all identifiers in a @case@ alternative.
instance HasIdentifiers (S.Alt a) where
  findIdentifiers (S.Alt _ pat rhs binds) = findIdentifiers pat
    `Set.union` findIdentifiers rhs
    `Set.union` findIdentifiers binds

-- | Collects all identifiers in @let@ and @where@ bindings.
instance HasIdentifiers (S.Binds a) where
  findIdentifiers (S.BDecls _ decls) = findIdentifiers decls

-- | Takes the identifier in a `S.QName`.
instance HasIdentifiers (S.QName a) where
  findIdentifiers (S.Qual _ _ name) = findIdentifiers name
  findIdentifiers (S.UnQual _ name) = findIdentifiers name
  findIdentifiers (S.Special _ _)   = Set.empty

-- | Takes the identifier of a name.
instance HasIdentifiers (S.Name a) where
  findIdentifiers (S.Ident _ s)  = Set.singleton s
  findIdentifiers (S.Symbol _ _) = Set.empty
