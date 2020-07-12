-- | This module applies the main pattern-matching compilation algorithm and
--   the different features to a Haskell module.

module HST.Application
  ( processModule
  , specialCons
  )
where

-- TODO too many variables generated
-- TODO only tuples supported

import           HST.CoreAlgorithm              ( match
                                                , err
                                                , isCons
                                                )
import           HST.Feature.CaseCompletion     ( applyCCModule )
import           HST.Environment.FreshVars      ( Constructor
                                                , PM
                                                , addConstrMap
                                                , opt
                                                , gets
                                                , newVars
                                                )
import           HST.Feature.GuardElimination   ( getMatchName
                                                , applyGEModule
                                                )
import           HST.Feature.Optimization       ( optimize )
import qualified HST.Frontend.Syntax           as S

-------------------------------------------------------------------------------
-- Application of Core Algorithm                                             --
-------------------------------------------------------------------------------

-- | Sequentially applies the different transformations to the given module
--   after initializing the environment with the data types declared in the
--   module.
--
--   Returns a new module with the transformed functions.
processModule :: S.EqAST a => S.Module a -> PM a (S.Module a)
processModule m = do
  collectDataInfo m
  eliminatedM    <- applyGEModule m
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
collectDataInfo :: S.Module a -> PM a ()
collectDataInfo (S.Module decls) = mapM_ collectDataDecl decls

-- | Inserts entries for the data type and constructors declared by the given
--   declaration into the environment.
--
--   Leaves the environment unchanged, if the given declaration is not a
--   data type declaration.
collectDataDecl :: S.Decl a -> PM a ()
collectDataDecl (S.DataDecl dhead cDecls) =
  addConstrMap (getDataName dhead, map getDataCons cDecls)
collectDataDecl _ = return ()

-- | Extracts the name of the data type declared by a data type declaration
--   with the given head.
getDataName :: S.DeclHead a -> String -- add symbols?
getDataName (S.DHead   dname) = fromName dname
getDataName (S.DHApp   decl ) = getDataName decl
getDataName (S.DHParen decl ) = getDataName decl
-- TODO Test symbols and infix
getDataName _ = error "getDataName: Symbol or infix in declaration"

-- | Inserts an entry for the given constructor declaration into the
--   environment.
getDataCons :: S.ConDecl a -> Constructor a
getDataCons (S.ConDecl cname types) =
  (S.UnQual S.NoSrcSpan cname, length types, False)
getDataCons (S.InfixConDecl _ cname _) = (S.UnQual S.NoSrcSpan cname, 2, True)
getDataCons (S.RecDecl _) = error "record notation is not supported"

-- | Extracts the identifier or symbol from the given name.
fromName :: S.Name a -> String
fromName (S.Ident  _ str) = str
fromName (S.Symbol _ str) = str

-- | Map of special constructors that cannot be defined in a module manually.
specialCons :: [(String, [Constructor a])]
specialCons =
  [ ("unit", [(S.Special S.NoSrcSpan (S.UnitCon S.NoSrcSpan), 0, False)])
  , ( "list"
    , [ (S.Special S.NoSrcSpan (S.ListCon S.NoSrcSpan), 0, False)
      , (S.Special S.NoSrcSpan (S.Cons S.NoSrcSpan)   , 2, True)
      ]
    )
  , ("fun", [(S.Special S.NoSrcSpan (S.FunCon S.NoSrcSpan), 2, True)])
  , ( "pair"
    , [(S.Special S.NoSrcSpan (S.TupleCon S.NoSrcSpan S.Boxed 2), 2, False)]
    )    -- TODO Tuples
  , ("wildcard", [(S.Special S.NoSrcSpan (S.ExprHole S.NoSrcSpan), 0, False)])
  ]
