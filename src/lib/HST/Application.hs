-- | This module applies the main pattern-matching compilation algorithm and
--   the different features to a Haskell module.
module HST.Application ( processModule ) where

-- TODO too many variables generated
-- TODO only tuples supported
import           Control.Monad                ( replicateM )
import           Control.Monad.Extra          ( ifM )
import           Polysemy                     ( Member, Members, Sem )

import           HST.CoreAlgorithm            ( Eqs, defaultErrorExp, match )
import           HST.Effect.Env               ( Env, modifyEnv )
import           HST.Effect.Fresh
  ( Fresh, freshVarPat, genericFreshPrefix )
import           HST.Effect.GetOpt            ( GetOpt, getOpt )
import           HST.Effect.Report            ( Report )
import           HST.Environment
  ( ConEntry(..), DataEntry(..), insertConEntry, insertDataEntry )
import           HST.Environment.Prelude      ( insertPreludeEntries )
import           HST.Feature.CaseCompletion   ( applyCCModule )
import           HST.Feature.GuardElimination ( applyGEModule, getMatchName )
import           HST.Feature.Optimization     ( optimize )
import qualified HST.Frontend.Syntax          as S
import           HST.Options                  ( optOptimizeCase )
import           HST.Util.Predicates          ( isConPat )
import           HST.Util.Selectors           ( expFromUnguardedRhs )

-------------------------------------------------------------------------------
-- Application of Core Algorithm                                             --
-------------------------------------------------------------------------------
-- | Sequentially applies the different transformations to the given module
--   after initializing the environment with the data types declared in the
--   module.
--
--   Returns a new module with the transformed functions.
processModule :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
              => S.Module a
              -> Sem r (S.Module a)
processModule m = do
  insertPreludeEntries
  collectDataInfo m
  guardEliminatedM <- applyGEModule m
  caseCompletedM <- applyCCModule guardEliminatedM
  useAlgoModule caseCompletedM

-- | Applies the core algorithm on each declaration in the given module.
useAlgoModule :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
              => S.Module a
              -> Sem r (S.Module a)
useAlgoModule (S.Module s origModuleHead moduleName decls) = do
  decls' <- mapM useAlgoDecl decls
  return $ S.Module s origModuleHead moduleName decls'

-- | Applies the core algorithm on the given declaration.
useAlgoDecl :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
            => S.Decl a
            -> Sem r (S.Decl a)
useAlgoDecl (S.FunBind _ ms) = do
  m' <- useAlgoMatches ms
  return (S.FunBind S.NoSrcSpan [m'])
useAlgoDecl v = return v

-- | Applies the core algorithm on a function declaration with the given
--   matches.
--
--   If the function has only one rule and no pattern is a constructor
--   pattern, the function is is left unchanged.
useAlgoMatches :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
               => [S.Match a]
               -> Sem r (S.Match a)
useAlgoMatches [m] | not (hasCons m) = return m
useAlgoMatches ms  = useAlgo ms

-- | Tests whether the given match of a function declaration contains
--   a constructor pattern.
hasCons :: S.Match a -> Bool
hasCons (S.Match _ _ ps _ _)         = any isConPat ps
hasCons (S.InfixMatch _ p1 _ ps _ _) = any isConPat (p1 : ps)

-- | Like 'useAlgoMatches' but applies the algorithm unconditionally.
useAlgo :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
        => [S.Match a]
        -> Sem r (S.Match a)
useAlgo ms = do
  eqs <- mapM matchToEquation ms
  let name  = getMatchName (head ms)
      arity = length (fst (head eqs))
  nVars <- replicateM arity (freshVarPat genericFreshPrefix)
  nExp <- match nVars eqs defaultErrorExp
  nExp' <- ifM (getOpt optOptimizeCase) (optimize nExp) (return nExp)
  return
    $ S.Match S.NoSrcSpan name nVars (S.UnGuardedRhs S.NoSrcSpan nExp') Nothing
 where
  -- | Converts a rule of a function declaration to an equation.
  --
  --   There must be no guards on the right-hand side.
  matchToEquation :: Member Report r => S.Match a -> Sem r (Eqs a)
  matchToEquation (S.Match _ _ pats rhs _)          = do
    expr <- expFromUnguardedRhs rhs
    return (pats, expr)
  matchToEquation (S.InfixMatch _ pat _ pats rhs _) = do
    expr <- expFromUnguardedRhs rhs
    return (pat : pats, expr)

-------------------------------------------------------------------------------
-- Environment Initialization                                                --
-------------------------------------------------------------------------------
-- | Initializes the environment with the data types declared in the given
--   module.
collectDataInfo :: Member (Env a) r => S.Module a -> Sem r ()
collectDataInfo (S.Module _ _ _ decls) = mapM_ collectDataDecl decls

-- | Inserts entries for the data type and constructors declared by the given
--   declaration into the environment.
--
--   Leaves the environment unchanged, if the given declaration is not a
--   data type declaration.
collectDataDecl :: Member (Env a) r => S.Decl a -> Sem r ()
collectDataDecl (S.DataDecl _ _ dataName conDecls) = do
  let dataQName  = S.UnQual S.NoSrcSpan dataName
      conEntries = map (makeConEntry dataQName) conDecls
  modifyEnv
    $ insertDataEntry DataEntry { dataEntryName = dataQName
                                , dataEntryCons = map conEntryName conEntries
                                }
  mapM_ (modifyEnv . insertConEntry) conEntries
collectDataDecl _ = return ()

-- | Creates an environment entry for a constructor declaration.
makeConEntry :: S.QName a -> S.ConDecl a -> ConEntry a
makeConEntry dataQName conDecl = ConEntry
  { conEntryName    = S.UnQual S.NoSrcSpan (S.conDeclName conDecl)
  , conEntryArity   = S.conDeclArity conDecl
  , conEntryIsInfix = S.conDeclIsInfix conDecl
  , conEntryType    = dataQName
  }
