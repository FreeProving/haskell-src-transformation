-- | This module applies the main pattern-matching compilation algorithm and
--   the different features to a Haskell module.
module HST.Application
  ( createModuleInterface
  , initializeEnvironment
  , processModule
  ) where

-- TODO too many variables generated
-- TODO only tuples supported
import           Control.Monad                ( zipWithM )
import           Control.Monad.Extra          ( ifM )
import           Data.List.Extra              ( groupSortOn )
import qualified Data.Map.Strict              as Map
import           Data.Maybe                   ( catMaybes, mapMaybe )
import           Polysemy                     ( Member, Members, Sem )

import           HST.CoreAlgorithm            ( Eqs, defaultErrorExp, match )
import           HST.Effect.Env               ( Env )
import           HST.Effect.Fresh
  ( Fresh, freshVarPatWithSrcSpan, genericFreshPrefix )
import           HST.Effect.GetOpt            ( GetOpt, getOpt )
import           HST.Effect.InputModule
  ( ConEntry(..), DataEntry(..), InputModule, ModuleInterface(..)
  , createConMapEntries, createDataMapEntry, getInputModule
  , getInputModuleInterface, getInputModuleInterfaceByName )
import           HST.Effect.Report            ( Report, report, reportFatal )
import           HST.Environment              ( Environment(..) )
import           HST.Environment.Prelude      ( preludeModuleInterface )
import           HST.Feature.CaseCompletion   ( applyCCModule )
import           HST.Feature.GuardElimination ( applyGEModule )
import           HST.Feature.Optimization     ( optimize )
import qualified HST.Frontend.Syntax          as S
import           HST.Options                  ( optOptimizeCase )
import           HST.Util.Messages
  ( Severity(Internal, Warning), message )
import           HST.Util.Predicates          ( isConPat )
import           HST.Util.PrettyName          ( prettyName )
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
  guardEliminatedM <- applyGEModule m
  caseCompletedM <- applyCCModule guardEliminatedM
  useAlgoModule caseCompletedM

-- | Applies the core algorithm on each declaration in the given module.
useAlgoModule :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
              => S.Module a
              -> Sem r (S.Module a)
useAlgoModule (S.Module s origModuleHead moduleName imports decls) = do
  decls' <- mapM useAlgoDecl decls
  return $ S.Module s origModuleHead moduleName imports decls'

-- | Applies the core algorithm on the given declaration.
useAlgoDecl :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
            => S.Decl a
            -> Sem r (S.Decl a)
useAlgoDecl (S.FunBind s ms) = do
  m' <- useAlgoMatches s ms
  return (S.FunBind s [m'])
useAlgoDecl v                = return v

-- | Applies the core algorithm on a function declaration with the given
--   matches.
--
--   If the function has only one rule and no pattern is a constructor
--   pattern, the function is is left unchanged.
useAlgoMatches :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
               => S.SrcSpan a
               -> [S.Match a]
               -> Sem r (S.Match a)
useAlgoMatches _ [m] | not (hasCons m) = return m
useAlgoMatches s ms  = useAlgo s ms

-- | Tests whether the given match of a function declaration contains
--   a constructor pattern.
hasCons :: S.Match a -> Bool
hasCons = any isConPat . S.matchPats

-- | Like 'useAlgoMatches' but applies the algorithm unconditionally.
useAlgo :: (Members '[Env a, Fresh, GetOpt, Report] r, S.EqAST a)
        => S.SrcSpan a
        -> [S.Match a]
        -> Sem r (S.Match a)
useAlgo s ms = do
  eqs <- mapM matchToEquation ms
  let name     = S.matchName (head ms)
      srcSpans = map S.getSrcSpan . S.matchPats . head $ ms
      isInfix  = all S.matchIsInfix ms
  nVars <- mapM (freshVarPatWithSrcSpan genericFreshPrefix) srcSpans
  nExp <- match nVars eqs defaultErrorExp
  nExp' <- ifM (getOpt optOptimizeCase) (optimize nExp) (return nExp)
  return $ S.Match s isInfix name nVars (S.UnGuardedRhs s nExp') Nothing
 where
  -- | Converts a rule of a function declaration to an equation.
  --
  --   There must be no guards on the right-hand side.
  matchToEquation :: Member Report r => S.Match a -> Sem r (Eqs a)
  matchToEquation (S.Match _ _ _ pats rhs _) = do
    expr <- expFromUnguardedRhs rhs
    return (pats, expr)

-------------------------------------------------------------------------------
-- Module Interface Creation                                                 --
-------------------------------------------------------------------------------
-- | Creates a module interface with the data types declared in the given
--   module.
createModuleInterface :: S.Module a -> ModuleInterface a
createModuleInterface (S.Module _ _ modName _ decls)
  = let interfaceEntries = mapMaybe createModuleInterfaceEntry decls
    in ModuleInterface { interfaceModName     = modName
                       , interfaceDataEntries = Map.fromList
                           (map createDataMapEntry interfaceEntries)
                       , interfaceConEntries  = Map.fromList
                           (concatMap createConMapEntries interfaceEntries)
                       }

-- | Creates a module interface entry for the data type and constructors
--   declared by the given declaration.
--
--   Returns @Nothing@ if the given declaration is not a data type declaration.
createModuleInterfaceEntry :: S.Decl a -> Maybe (DataEntry a)
createModuleInterfaceEntry (S.DataDecl _ _ dataName conDecls)
  = let dataQName  = S.unQual dataName
        conEntries = map (makeConEntry dataQName) conDecls
    in Just (DataEntry dataQName conEntries)
createModuleInterfaceEntry _ = Nothing

-- | Creates a module interface entry for a constructor declaration.
makeConEntry :: S.QName a -> S.ConDecl a -> ConEntry a
makeConEntry dataQName conDecl = ConEntry
  { conEntryName    = S.unQual (S.conDeclName conDecl)
  , conEntryArity   = S.conDeclArity conDecl
  , conEntryIsInfix = S.conDeclIsInfix conDecl
  , conEntryType    = dataQName
  }

-------------------------------------------------------------------------------
-- Environment Initialization                                                --
-------------------------------------------------------------------------------
-- | Initializes the environment for the module stored at the given file path.
--
--   The module interfaces of imported modules are added to the environment.
--   If such a module is unavailable, the import is skipped and the user is
--   informed about the skip.
initializeEnvironment
  :: Members '[InputModule a, Report] r => FilePath -> Sem r (Environment a)
initializeEnvironment filePath = do
  S.Module _ _ _ imports _ <- getInputModule filePath
  currentModule <- getInputModuleInterface filePath
  mInterfaces <- mapM (getInputModuleInterfaceByName . S.importModule) imports
  mImportedModules <- zipWithM reportMissingModule imports mInterfaces
  importedModules <- mapM collectImportDecls
    $ groupSortOn (interfaceModName . snd) (catMaybes mImportedModules)
  return Environment { envCurrentModule   = currentModule
                     , envImportedModules = importedModules
                     , envOtherEntries    = preludeModuleInterface
                     }
 where
  -- | Returns the given import declaration and module interface, if the module
  --   could be successfully be imported, or reports the skip of the missing
  --   module and returns @Nothing@.
  reportMissingModule :: Member Report r
                      => S.ImportDecl a
                      -> Maybe (ModuleInterface a)
                      -> Sem r (Maybe (S.ImportDecl a, ModuleInterface a))
  reportMissingModule importDecl Nothing                = do
    report
      $ message Warning (S.importSrcSpan importDecl)
      $ "The module `"
      ++ prettyName (S.importModule importDecl)
      ++ "` could not be found and the import is skipped!"
    return Nothing
  reportMissingModule importDecl (Just moduleInterface)
    = return $ Just (importDecl, moduleInterface)

  -- | Collects all import declarations in the given list of pairs of import
  --   declarations and module interfaces and returns a single pair with these
  --   import declarations and the module interface of the first entry of the
  --   given list.
  --
  --   This function is meant to collect multiple import declarations referring
  --   to the same module.
  collectImportDecls :: Member Report r
                     => [(S.ImportDecl a, ModuleInterface a)]
                     -> Sem r ([S.ImportDecl a], ModuleInterface a)
  collectImportDecls imports@((_, interface) : _)
    = return (map fst imports, interface)
  collectImportDecls [] = reportFatal
    $ message Internal S.NoSrcSpan
    $ "`collectImportDecls` was called on an empty list!"
