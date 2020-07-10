-- | This module applies the main pattern-matching compilation algorithm and
--   the different features to a Haskell module.

module HST.Application
  ( processModule
  , specialCons
  , useAlgoModule
  )
where                                      -- TODO too many variables generated
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
import           HST.Feature.GuardElimination   ( comp
                                                , getMatchName
                                                , applyGEModule
                                                )
import           HST.Feature.Optimization       ( optimize )
import qualified HST.Frontend.Syntax           as S

-- | The function 'useAlgo' applies the algorithm on each declaration in
--   the module.
useAlgoModule :: S.EqAST a => S.Module a -> PM a (S.Module a)
useAlgoModule (S.Module ds) = do
  dcls <- mapM useAlgoDecl ds
  return $ S.Module dcls

-- | The function 'useAlgoDecl' applies the algorithm on the the FunBinds
useAlgoDecl :: S.EqAST a => S.Decl a -> PM a (S.Decl a)
useAlgoDecl (S.FunBind _ ms) = do
  nms <- useAlgoMatches ms
  return (S.FunBind S.NoSrcSpan nms)
useAlgoDecl v = return v

-- TODO maybe refactor to fun decl or check if oneFun stuff is needed or
-- always true
useAlgoMatches :: S.EqAST a => [S.Match a] -> PM a [S.Match a]
useAlgoMatches []       = return []
useAlgoMatches (m : ms) = do
  let (oneFun, r) = span (comp m) ms
  if not (null oneFun) || hasCons (m : oneFun)
    then do
      x  <- useAlgo (m : oneFun)
      xs <- useAlgoMatches r
      return (x : xs)
    else do
      xs <- useAlgoMatches r
      return (m : xs)

-- | Checks a given list of Matches for constructor pattern.
--   Returns True if the list contains more than one Match or if any of the
--   pattern is a constructor pattern.
hasCons :: [S.Match a] -> Bool
hasCons [m] = case m of
  S.Match _ _ ps _ _         -> any isCons ps
  S.InfixMatch _ p1 _ ps _ _ -> any isCons (p1 : ps)
hasCons _ = True -- False?

-- | The function 'useAlgo' applies the match function to a list of matches
--   returning a single Match.
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

-- a general version of add
addG :: (b -> PM a ()) -> Maybe b -> PM a ()
addG = maybe (return ())
-- addG f ma = maybe (return()) f ma

-- | The function 'collectDataInfo' takes a module and writes all datatype
--   declarations into the State with their name and constructors.
collectDataInfo :: S.Module a -> PM a ()
collectDataInfo (S.Module decls) = do
  mas <- mapM collectDataDecl decls
  mapM_ (addG addConstrMap) mas

-- | The function 'collectDataDecl' takes a Declaration and returns a pair of
--   a datatype name and a list of cunstructors if the declaration was a
--   DataDecl. Returns Nothing otherwise.
collectDataDecl :: S.Decl a -> PM a (Maybe (String, [Constructor a]))
collectDataDecl (S.DataDecl dhead cDecls) =
  return $ Just (getDataName dhead, map getDataCons cDecls)
collectDataDecl _ = return Nothing

-- | The function 'getDataName' takes a DeclHead and returns a string with the
--   name of the data type.
getDataName :: S.DeclHead a -> String -- add symbols?
getDataName (S.DHead   dname) = fromName dname
getDataName (S.DHApp   decl ) = getDataName decl
getDataName (S.DHParen decl ) = getDataName decl
-- TODO Test symbols and infix
getDataName _ = error "getDataName: Symbol or infix in declaration"

-- | The function 'getDataName' takes a QualConDecl and returns the contained
--   constructor.
getDataCons :: S.ConDecl a -> Constructor a
getDataCons (S.ConDecl cname types) =
  (S.UnQual S.NoSrcSpan cname, length types, False)
getDataCons (S.InfixConDecl _ cname _) = (S.UnQual S.NoSrcSpan cname, 2, True)
getDataCons (S.RecDecl _) = error "record notation is not supported"

-- |The function 'fromName' takes a Name and returns its String.
fromName :: S.Name a -> String
fromName (S.Ident  _ str) = str
fromName (S.Symbol _ str) = str

-- | The function 'processModule' sequentially applies the different
--   transformations to the given module after collecting the data types.
--   Returns a new module with the transformed functions.
processModule :: S.EqAST a => S.Module a -> PM a (S.Module a)
processModule m = do
  collectDataInfo m -- TODO  maybe unused
  eliminatedM    <- applyGEModule m
  caseCompletedM <- applyCCModule eliminatedM
  useAlgoModule caseCompletedM

-- | 'specialCons' is a map for the sugared data types in Haskell, since they
--   can not be defined in a module by hand.
--   This map is the default 'FreshVars.constrMap' for the 'FreshVars.PMState'
--   used in @Main.hs@
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
