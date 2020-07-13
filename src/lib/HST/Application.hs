-- | This module applies the main pattern-matching compilation algorithm and
--   the different features to a Haskell module.

module HST.Application
  ( processModule
  )
where

-- TODO too many variables generated
-- TODO only tuples supported

import           Polysemy                       ( Member
                                                , Members
                                                , Sem
                                                )

import           HST.CoreAlgorithm              ( match
                                                , err
                                                , isCons
                                                )
import           HST.Effect.Env                 ( Env
                                                , modifyEnv
                                                , inEnv
                                                )
import           HST.Effect.Fresh               ( Fresh
                                                , freshIndex
                                                , genericFreshPrefix
                                                )
import           HST.Effect.GetOpt              ( GetOpt
                                                , getOpt
                                                )
import           HST.Environment                ( ConEntry(..)
                                                , DataEntry(..)
                                                , insertConEntry
                                                , insertDataEntry
                                                , envToConstrMap
                                                )
import           HST.Environment.FreshVars      ( Constructor
                                                , PMState(..)
                                                , PM
                                                , evalPM
                                                , opt
                                                , gets
                                                , newVars
                                                )
import           HST.Environment.Prelude        ( insertPreludeEntries )
import           HST.Feature.CaseCompletion     ( applyCCModule )
import           HST.Feature.GuardElimination   ( getMatchName
                                                , applyGEModule
                                                )
import           HST.Feature.Optimization       ( optimize )
import qualified HST.Frontend.Syntax           as S
import           HST.Options                    ( optTrivialCase
                                                , optOptimizeCase
                                                )

-------------------------------------------------------------------------------
-- Application of Core Algorithm                                             --
-------------------------------------------------------------------------------

-- | Sequentially applies the different transformations to the given module
--   after initializing the environment with the data types declared in the
--   module.
--
--   Returns a new module with the transformed functions.
processModule
  :: (Members '[Env a, Fresh, GetOpt] r, S.EqAST a)
  => S.Module a
  -> Sem r (S.Module a)
processModule m = do
  insertPreludeEntries
  collectDataInfo m
  eliminatedM <- applyGEModule m
  state       <- initPMState
  return $ flip evalPM state $ do
    caseCompletedM <- applyCCModule eliminatedM
    useAlgoModule caseCompletedM

-- | Applies the core algorithm on each declaration in the given module.
useAlgoModule :: S.EqAST a => S.Module a -> PM a (S.Module a)
useAlgoModule (S.Module ds) = do
  dcls <- mapM useAlgoDecl ds
  return $ S.Module dcls

-- | Applies the core algorithm on the given declaration.
useAlgoDecl :: S.EqAST a => S.Decl a -> PM a (S.Decl a)
useAlgoDecl (S.FunBind _ ms) = do
  m' <- useAlgoMatches ms
  return (S.FunBind S.NoSrcSpan [m'])
useAlgoDecl v = return v

-- | Applies the core algorithm on a function declaration with the given
--   matches.
--
--   If the function has only one rule and no pattern is a constructor
--   pattern, the algorithm is is left unchanged.
useAlgoMatches :: S.EqAST a => [S.Match a] -> PM a (S.Match a)
useAlgoMatches [m] | not (hasCons m) = return m
useAlgoMatches ms                    = useAlgo ms

-- | Tests whether the given match of a function declaration contains
--   a constructor pattern.
hasCons :: S.Match a -> Bool
hasCons (S.Match _ _ ps _ _        ) = any isCons ps
hasCons (S.InfixMatch _ p1 _ ps _ _) = any isCons (p1 : ps)

-- | Like 'useAlgoMatches' but applies the algorithm unconditionally.
useAlgo
  :: S.EqAST a
  => [S.Match a]          -- all matches for one function name
  -> PM a (S.Match a) -- contains one match
useAlgo ms = do
  let mname    = getMatchName ms
  let eqs = map (\(S.Match _ _ pats rhs _) -> (pats, selectExp rhs)) ms
  let funArity = (length . fst . head) eqs
  nVars <- newVars funArity
  nExp  <- match nVars eqs err
  doOpt <- gets opt
  nExp' <- if doOpt then optimize nExp else return nExp
  return $ S.Match S.NoSrcSpan
                   mname
                   nVars
                   (S.UnGuardedRhs S.NoSrcSpan nExp')
                   Nothing
 where
  selectExp :: S.Rhs a -> S.Exp a
  selectExp (S.UnGuardedRhs _ e) = e
  selectExp _                    = error "no UnGuardedRhs in selectExp"

-------------------------------------------------------------------------------
-- Environment Initialization                                                --
-------------------------------------------------------------------------------

-- | Initializes the environment with the data types declared in the given
--   module.
collectDataInfo :: Member (Env a) r => S.Module a -> Sem r ()
collectDataInfo (S.Module decls) = mapM_ collectDataDecl decls

-- | Inserts entries for the data type and constructors declared by the given
--   declaration into the environment.
--
--   Leaves the environment unchanged, if the given declaration is not a
--   data type declaration.
collectDataDecl :: Member (Env a) r => S.Decl a -> Sem r ()
collectDataDecl (S.DataDecl dataName conDecls) = do
  let dataQName  = S.UnQual S.NoSrcSpan dataName
      conEntries = map (makeConEntry dataQName) conDecls
  modifyEnv $ insertDataEntry DataEntry
    { dataEntryName = dataQName
    , dataEntryCons = map conEntryName conEntries
    }
  mapM_ (modifyEnv . insertConEntry) conEntries
collectDataDecl _ = return ()

-- | Creates an environment entry for a constructor declaration .
makeConEntry :: S.QName a -> S.ConDecl a -> ConEntry a
makeConEntry dataQName (S.ConDecl cname types) = ConEntry
  { conEntryName    = S.UnQual S.NoSrcSpan cname
  , conEntryArity   = length types
  , conEntryIsInfix = False
  , conEntryType    = dataQName
  }
makeConEntry dataQName (S.InfixConDecl _ cname _) = ConEntry
  { conEntryName    = S.UnQual S.NoSrcSpan cname
  , conEntryArity   = 2
  , conEntryIsInfix = True
  , conEntryType    = dataQName
  }

-------------------------------------------------------------------------------
-- Backward Compatibility                                                    --
-------------------------------------------------------------------------------

-- | Creates the initial 'PMState' from the given command line options.
initPMState :: Members '[Env a, Fresh, GetOpt] r => Sem r (PMState a)
initPMState = do
  trivialCase  <- getOpt optTrivialCase
  optimizeCase <- getOpt optOptimizeCase
  constrMap'   <- initConstrMap
  nextId'      <- freshIndex genericFreshPrefix
  return $ PMState { nextId     = nextId'
                   , constrMap  = constrMap'
                   , matchedPat = []
                   , trivialCC  = trivialCase
                   , opt        = optimizeCase
                   }

-- | Creates the 'constrMap' of the 'PMState' created by 'initPMState'.
initConstrMap :: Member (Env a) r => Sem r [(String, [Constructor a])]
initConstrMap = inEnv envToConstrMap
