-- | This module applies the main pattern-matching compilation algorithm and
--   the different features to a Haskell module.

module HST.Application
  ( processModule
  , specialCons
  , useAlgoModule
  )
where

-- TODO too many variables generated
-- TODO only tuples supported

import           HST.CoreAlgorithm              ( match
                                                , err
                                                , isCons
                                                )
import qualified HST.Feature.CaseCompletion    as CC
                                                ( applyCCModule )
import           HST.Environment.FreshVars      ( Constructor
                                                , PM
                                                , addConstrMap
                                                , opt
                                                , gets
                                                , newVars
                                                )
import qualified HST.Feature.GuardElimination  as GE
                                                ( comp
                                                , getMatchName
                                                , applyGEModule
                                                )
import           HST.Feature.Optimization       ( optimize )

import qualified Language.Haskell.Exts.Syntax  as HSE

-- | The function 'useAlgo' applies the algorithm on each declaration in
--   the module.
useAlgoModule :: HSE.Module () -> PM (HSE.Module ())
useAlgoModule (HSE.Module _ mmh mps ids ds) = do
  dcls <- mapM useAlgoDecl ds
  return $ HSE.Module () mmh mps ids dcls
useAlgoModule _ = error "useAlgoModule: not on module"

-- | The function 'useAlgoDecl' applies the algorithm on the the FunBinds
useAlgoDecl :: HSE.Decl () -> PM (HSE.Decl ())
useAlgoDecl (HSE.FunBind _ ms) = do
  nms <- useAlgoMatches ms
  return (HSE.FunBind () nms)
useAlgoDecl v = return v

-- TODO maybe refactor to fun decl or check if oneFun stuff is needed or
-- always true
useAlgoMatches :: [HSE.Match ()] -> PM [HSE.Match ()]
useAlgoMatches []       = return []
useAlgoMatches (m : ms) = do
  let (oneFun, r) = span (GE.comp m) ms
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
hasCons :: [HSE.Match ()] -> Bool
hasCons [m] = case m of
  HSE.Match _ _ ps _ _         -> any isCons ps
  HSE.InfixMatch _ p1 _ ps _ _ -> any isCons (p1 : ps)
hasCons _ = True -- False?

-- | The function 'useAlgo' applies the match function to a list of matches
--   returning a single Match.
useAlgo
  :: [HSE.Match ()]    -- all matches for one function name
  -> PM (HSE.Match ()) -- contains one match
useAlgo ms = do
  let mname    = GE.getMatchName ms
  let eqs = map (\(HSE.Match _ _ pats rhs _) -> (pats, selectExp rhs)) ms
  let funArity = (length . fst . head) eqs
  nVars <- newVars funArity
  nExp  <- match nVars eqs err
  b     <- gets opt
  if b
    then do
      oExp <- optimize nExp
      return $ HSE.Match () mname nVars (HSE.UnGuardedRhs () oExp) Nothing
    else return $ HSE.Match () mname nVars (HSE.UnGuardedRhs () nExp) Nothing
 where
  selectExp :: HSE.Rhs () -> HSE.Exp ()
  selectExp (HSE.UnGuardedRhs _ e) = e
  selectExp _                      = error "no UnGuardedRhs in selectExp"

-- a general version of add
addG :: (a -> PM ()) -> Maybe a -> PM ()
addG = maybe (return ())
-- addG f ma = maybe (return()) f ma

-- | The function 'collectDataInfo' takes a module and writes all datatype
--   declarations into the State with their name and constructors.
collectDataInfo :: HSE.Module () -> PM ()
collectDataInfo (HSE.Module _ _ _ _ decls) = do
  mas <- mapM collectDataDecl decls
  mapM_ (addG addConstrMap) mas
collectDataInfo _ = return ()

-- | The function 'collectDataDecl' takes a Declaration and returns a pair of
--   a datatype name and a list of cunstructors if the declaration was a
--   DataDecl. Returns Nothing otherwise.
collectDataDecl :: HSE.Decl () -> PM (Maybe (String, [Constructor]))
collectDataDecl (HSE.DataDecl _ (HSE.DataType _) _ dhead qcdecls _) =
  return $ Just (getDataName dhead, map getDataCons qcdecls)
collectDataDecl _ = return Nothing

-- | The function 'getDataName' takes a DeclHead and returns a string with the
--   name of the data type.
getDataName :: HSE.DeclHead () -> String -- add symbols?
getDataName (HSE.DHead _ dname ) = fromName dname
getDataName (HSE.DHApp _ decl _) = getDataName decl
getDataName (HSE.DHParen _ decl) = getDataName decl
-- TODO Test symbols and infix
getDataName _ = error "getDataName: Symbol or infix in declaration"

-- | The function 'getDataName' takes a QualConDecl and returns the contained
--   constructor.
getDataCons :: HSE.QualConDecl () -> Constructor
getDataCons (HSE.QualConDecl _ _ _ cdecl) = getDataCons' cdecl
 where
  getDataCons' :: HSE.ConDecl () -> Constructor
  getDataCons' (HSE.ConDecl _ cname types) =
    (HSE.UnQual () cname, length types, False)
  getDataCons' (HSE.InfixConDecl _ _ cname _) = (HSE.UnQual () cname, 2, True)
  getDataCons' (HSE.RecDecl _ _ _) = error "record notation is not supported"

-- |The function 'fromName' takes a Name and returns its String.
fromName :: HSE.Name () -> String
fromName (HSE.Ident  _ str) = str
fromName (HSE.Symbol _ str) = str

-- | The function 'processModule' sequentially applies the different
--   transformations to the given module after collecting the data types.
--   Returns a new module with the transformed functions.
processModule :: HSE.Module () -> PM (HSE.Module ())
processModule m = do
  collectDataInfo m -- TODO  maybe unused
  eliminatedM    <- GE.applyGEModule m
  caseCompletedM <- CC.applyCCModule eliminatedM
  useAlgoModule caseCompletedM

-- | 'specialCons' is a map for the sugared data types in Haskell, since they
--   can not be defined in a module by hand.
--   This map is the default 'FreshVars.constrMap' for the 'FreshVars.PMState'
--   used in @Main.hs@
specialCons :: [(String, [Constructor])]
specialCons =
  [ ("unit", [(HSE.Special () (HSE.UnitCon ()), 0, False)])
  , ( "list"
    , [ (HSE.Special () (HSE.ListCon ()), 0, False)
      , (HSE.Special () (HSE.Cons ())   , 2, True)
      ]
    )
  , ("fun", [(HSE.Special () (HSE.FunCon ()), 2, True)])
  , ( "pair"
    , [(HSE.Special () (HSE.TupleCon () HSE.Boxed 2), 2, False)]
    )    -- TODO Tuples
  , ("wildcard", [(HSE.Special () (HSE.ExprHole ()), 0, False)])
  ]
