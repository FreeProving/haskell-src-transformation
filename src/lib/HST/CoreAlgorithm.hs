-- | This module contains the actual implementation of the pattern-matching
--   compilation algorithm.

module HST.CoreAlgorithm
  ( match
  , defaultErrorExp
  , translatePVar
  , Eqs
  , isPVar
  , isCons
  , getPVarName
  , getQName
  , getQNamePat
  -- * Testing interface
  , compareCons
  )
where

import           Control.Monad                  ( replicateM )
import           Data.List                      ( (\\)
                                                , partition
                                                , groupBy
                                                )
import           Data.Maybe                     ( fromJust )
import           Data.Function                  ( on )
import           Polysemy                       ( Member
                                                , Members
                                                , Sem
                                                )

import           HST.Effect.Env                 ( Env
                                                , inEnv
                                                )
import           HST.Effect.Fresh               ( Fresh
                                                , freshIdent
                                                , freshVarPat
                                                , genericFreshPrefix
                                                )
import           HST.Effect.GetOpt              ( GetOpt
                                                , getOpt
                                                )
import           HST.Environment                ( ConEntry
                                                , DataEntry
                                                , conEntryArity
                                                , conEntryIsInfix
                                                , conEntryName
                                                , conEntryType
                                                , dataEntryCons
                                                , lookupConEntry
                                                , lookupDataEntry
                                                )
import           HST.Environment.Renaming       ( subst
                                                , tSubst
                                                , rename
                                                , substitute
                                                )
import qualified HST.Frontend.Syntax           as S
import           HST.Options                    ( optTrivialCase )

-- | A type that represents a single equation of a function declaration.
--
--   An equation is characterized by the argument patterns and the right-hand
--   side.
type Eqs a = ([S.Pat a], S.Exp a)

-- | The default error expression to insert for pattern matching failures.
defaultErrorExp :: S.Exp a
defaultErrorExp =
  S.Var S.NoSrcSpan (S.UnQual S.NoSrcSpan (S.Ident S.NoSrcSpan "undefined"))

-- | Compiles the given equations of a function declaration to a single
--   expression that performs explicit pattern matching using @case@
--   expressions.
--
--   All equations must have the same number of patterns as the given list
--   of fresh variable patterns.
match
  :: (Members '[Env a, Fresh, GetOpt] r, S.EqAST a)
  => [S.Pat a] -- ^ Fresh variable patterns.
  -> [Eqs a]   -- ^ The equations of the function declaration.
  -> S.Exp a   -- ^ The error expression for pattern-matching failures.
  -> Sem r (S.Exp a)
match [] (([], e) : _) _  = return e  -- Rule 3a: All patterns matched.
match [] []            er = return er -- Rule 3b: Pattern-matching failure.
match vars@(x : xs) eqs er
  | -- Rule 1: Pattern list of all equations starts with a variable pattern.
    allVars eqs = do
    eqs' <- mapM (substVars x) eqs
    match xs eqs' er
  | -- Rule 2: Pattern lists of all equations starts with a constructor pattern.
    allCons eqs = makeRhs x xs eqs er
  | -- Rule 4: Pattern lists of some equations start with a variable pattern
    -- and others start with a constructor pattern.
    --
    -- TODO This is probably causing the infinite loops when there are
    --      unsupported patterns.
    otherwise = createRekMatch vars er (groupPat eqs)
match [] _ _ = error "match: equations have different number of arguments"

-- | Substitutes all occurrences of the variable bound by the first pattern
--   (must be a variable or wildcard pattern) of the given equation by the
--   fresh variable bound by the given variable pattern on the right-hand side
--   of the equation.
substVars :: Member Fresh r => S.Pat a -> Eqs a -> Sem r (Eqs a)
substVars pv (p : ps, e) = do
  s1 <- getPVarName p
  s2 <- getPVarName pv
  let sub = subst s1 s2
  return (ps, rename sub e)
substVars _ _ = error "substVars: expected equation with at least one pattern"

-- | Extracts the name of the variable bound by the given variable pattern.
--
--   Returns a fresh variable for wildcard patterns.
--   The given pattern must be a variable or wildcard pattern.
getPVarName :: Member Fresh r => S.Pat a -> Sem r String
getPVarName (S.PVar _ pname) = getNameStr pname
 where
  getNameStr (S.Ident  _ str) = return str
  getNameStr (S.Symbol _ str) = return str
getPVarName (S.PWildCard _) = freshIdent genericFreshPrefix
getPVarName _ = error "getPVarName: expected variable or wildcard pattern"

-- | Groups the given equations based on the type of their first pattern.
--
--   The order of the equations is not changed, i.e., concatenation of the
--   resulting groups yields the original list of equations.

--   Two equations are in the same group only if their pattern lists both
--   start with a variable pattern or both start with a constructor pattern.
groupPat :: [Eqs a] -> [[Eqs a]]
groupPat = groupBy ordPats
  where ordPats (ps, _) (qs, _) = isPVar (head ps) && isPVar (head qs)

-- | Tests whether the given pattern is a variable pattern.
--
--   TODO shouldn't this return @True@ for wildcard patterns, too?
--   What's the difference to @isVar@?
isPVar :: S.Pat a -> Bool
isPVar (S.PVar _ _) = True
isPVar _            = False

-- | Applies 'match' to every group of equations where the error expression
--   is the 'match' result of the next group.
createRekMatch
  :: (Members '[Env a, Fresh, GetOpt] r, S.EqAST a)
  => [S.Pat a] -- ^ Fresh variable patterns.
  -> S.Exp a   -- ^ The error expression for pattern-matching failures.
  -> [[Eqs a]] -- ^ Groups of equations (see 'groupPat').
  -> Sem r (S.Exp a)
createRekMatch vars er =
  foldr (\eqs mrhs -> mrhs >>= match vars eqs) (return er)

-- | Creates a case expression that performs pattern matching on the variable
--   bound by the given variable pattern.
makeRhs
  :: (Members '[Env a, Fresh, GetOpt] r, S.EqAST a)
  => S.Pat a   -- ^ The fresh variable pattern to match.
  -> [S.Pat a] -- ^ The remaing fresh variable patterns.
  -> [Eqs a]   -- ^ The equations.
  -> S.Exp a   -- ^ The error expression for pattern-matching failures.
  -> Sem r (S.Exp a)
makeRhs x xs eqs er = do
  alts <- computeAlts x xs eqs er
  return (S.Case S.NoSrcSpan (translatePVar x) alts)

-- | Converts the given variable pattern to a variable expression.
translatePVar :: S.Pat a -> S.Exp a
translatePVar (S.PVar _ vname) = S.var vname
translatePVar _ = error "translatePVar: expected variable pattern" --, got" ++ show p)

-- TODO remove redundand types

-- | Generates @case@ expression alternatives for the given equations.
--
--   If there are missing constructors and trivial case completion is enabled,
--   an alternative with an wildcard pattern is added.
--   If trivial case completion is not enabled, one alternative is added for
--   every missing constructor.
computeAlts
  :: (Members '[Env a, Fresh, GetOpt] r, S.EqAST a)
  => S.Pat a   -- ^ The variable pattern that binds the matched variable.
  -> [S.Pat a] -- ^ The remaing fresh variable patterns.
  -> [Eqs a]   -- ^ The equations to generate alternatives for.
  -> S.Exp a   -- ^ The error expression for pattern-matching failures.
  -> Sem r [S.Alt a]
computeAlts x xs eqs er = do
  alts <- mapM (computeAlt x xs er) (groupByCons eqs)
  mxs  <- getMissingConstrs alts
  case mxs of
    [] -> return alts
    zs -> do
      b <- getOpt optTrivialCase
      if b
        then -- TODO is 'defaultErrorExp' correct? Why not 'er'?
             return $ alts ++ [S.alt (S.PWildCard S.NoSrcSpan) defaultErrorExp]
        else do
          z <- createAltsFromConstr x zs er
          -- TODO currently not sorted (reversed)
          return $ alts ++ z

-- | Looks up the constructors of the data type that is matched by the given
--   @case@ expression alternatives for which there are no alternatives already.
getMissingConstrs :: Member (Env a) r => [S.Alt a] -> Sem r [ConEntry a]
getMissingConstrs []   = error "getMissingConstrs: empty case"
getMissingConstrs alts = do
  let matchedConNames = map getQName alts
  dataEntry <- findDataEntry (head matchedConNames)
  let missingConNames = dataEntryCons dataEntry \\ matchedConNames
  mapM (fmap fromJust . inEnv . lookupConEntry) missingConNames

-- | Looks up the data type for the given constructor in the given environment.
findDataEntry :: Member (Env a) r => S.QName a -> Sem r (DataEntry a)
findDataEntry conName = do
  maybeDataName <- inEnv $ fmap conEntryType . lookupConEntry conName
  case maybeDataName of
    Nothing       -> error "findDataType: constructor not in scope"
    Just dataName -> do
      maybeDataEntry <- inEnv $ lookupDataEntry dataName
      case maybeDataEntry of
        Nothing        -> error "findDataType: data type not in scope"
        Just dataEntry -> return dataEntry

-- | Gets the name of the constructor matched by the given @case@ expression
--   alternative.
getQName :: S.Alt a -> S.QName a
getQName (S.Alt _ p _ _) = getQNamePat p

-- | Gets the name of a constructor pattern.
--
--   Returns the 'S.Special' names for special patterns such as lists and tuples.
getQNamePat :: S.Pat a -> S.QName a
getQNamePat (S.PApp _ qn _) = qn
getQNamePat (S.PInfixApp _ _ qn _) = qn
getQNamePat (S.PList _ _) = S.Special S.NoSrcSpan (S.ListCon S.NoSrcSpan)
getQNamePat (S.PWildCard _) = S.Special S.NoSrcSpan (S.ExprHole S.NoSrcSpan) -- TODO aren't wildcard pattern considers variable and not constructor patterns?
getQNamePat (S.PTuple _ bxd ps) =
  S.Special S.NoSrcSpan (S.TupleCon S.NoSrcSpan bxd (length ps))
getQNamePat _ = error "getQNamePat unsuported Pattern"

-- TODO refactor with smartcons

-- | Creates new @case@ expression alternatives for the given missing
--   constructors.
createAltsFromConstr
  :: (Member Fresh r, S.EqAST a)
  => S.Pat a         -- ^ The fresh variable matched by the @case@ expression.
  -> [ConEntry a]    -- ^ The missing constructors to generate alternatives for.
  -> S.Exp a         -- ^ The error expression for pattern-matching failures.
  -> Sem r [S.Alt a]
createAltsFromConstr x cs er = mapM (createAltFromConstr x er) cs
 where
  createAltFromConstr
    :: (Member Fresh r, S.EqAST a)
    => S.Pat a
    -> S.Exp a
    -> ConEntry a
    -> Sem r (S.Alt a)
  createAltFromConstr pat e conEntry = do
    nvars <- replicateM (conEntryArity conEntry)
                        (freshVarPat genericFreshPrefix)
    let p
          | conEntryIsInfix conEntry = S.PInfixApp S.NoSrcSpan
                                                   (head nvars)
                                                   (conEntryName conEntry)
                                                   (nvars !! 1)
          | otherwise = S.PApp S.NoSrcSpan (conEntryName conEntry) nvars
        p'   = translateApp p
        pat' = translatePVar pat
        e'   = substitute (tSubst pat' p') e
    return (S.alt p e')

-- | Groups the given equations based on the constructor matched by their
--   first pattern.
--
--   The order of equations that match the same constructor (i.e., within
--   groups) is preserved.
groupByCons :: [Eqs a] -> [[Eqs a]]
groupByCons = groupBy2 select

-- | A version of 'groupBy' that produces only one group for all
--   elements that are equivalent under the given predicate.
--
--   For example
--
--   > groupBy2 ((==) `on` fst)
--   >          [('a', 1), ('a', 2), ('b', 3), ('a', 4), ('b', 5)]
--
--   yields the following groups
--
--   > [[('a',1),('a',2),('a',4)],[('b',3),('b',5)]]
--
--   whereas 'groupBy' would have returned the following.
--
--   > [[('a',1),('a',2)],[('b',3)],[('a',4)],[('b',5)]]
--
--   This function is stable as the order of elements within each group
--   is preserved.
groupBy2 :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy2 = groupBy2' []
 where
  groupBy2' :: [[a]] -> (a -> a -> Bool) -> [a] -> [[a]]
  groupBy2' acc _ [] = reverse acc -- TODO makes acc redundant.. refactor
  groupBy2' acc comp (x : xs) =
    let (ys, zs) = partition (comp x) xs in groupBy2' ((x : ys) : acc) comp zs

-- | Tests whether the pattern lists of the given equations start with the same
--   constructor.
select :: Eqs a -> Eqs a -> Bool
select (ps, _) (qs, _) = compareCons (head ps) (head qs)

-- | Tests whether the given patterns match the same constructor or both match
--   the wildcard pattern.
compareCons :: S.Pat a -> S.Pat a -> Bool
compareCons = (==) `on` consName

-- | Returns the qualified name of the constructor of the given pattern or
--   @Nothing@ if it is a wildcard pattern.
consName :: S.Pat a -> Maybe (S.QName a)
consName (S.PApp _ qn _       ) = return qn
consName (S.PInfixApp _ _ qn _) = return qn
consName (S.PParen _ pat      ) = consName pat
consName (S.PList _ []) =
  return $ S.Special S.NoSrcSpan $ S.ListCon S.NoSrcSpan
consName (S.PList _ (_ : _)) =
  return $ S.Special S.NoSrcSpan $ S.Cons S.NoSrcSpan
consName (S.PTuple _ bxd ps) =
  return $ S.Special S.NoSrcSpan $ S.TupleCon S.NoSrcSpan bxd $ length ps
consName (S.PWildCard _) = Nothing
consName _               = error "consName: unsupported pattern" -- \"" ++ P.prettyPrint pat ++ "\""

-- | Creates an alternative for a @case@ expression for the given group of
--   equations whose first pattern matches the same constructor.
--
--   The variable matched by the case expression is substituted by the
--   expression that corresponds to the pattern of the alternative
--   in order to preserve "linearity". This is an optimization that was
--   originally implemented for the free-compiler to compensate the
--   lack of sharing. This optimization turned out to confuse the
--   termination check of the free-compiler and can probably be removed
--   once sharing is implemented.
computeAlt
  :: (Members '[Env a, Fresh, GetOpt] r, S.EqAST a)
  => S.Pat a   -- ^ The variable pattern that binds the matched variable.
  -> [S.Pat a] -- ^ The remaing fresh variable patterns.
  -> S.Exp a   -- ^ The error expression for pattern-matching failures.
  -> [Eqs a]   -- ^ A group of equations (see 'groupByCons').
  -> Sem r (S.Alt a)
computeAlt _   _    _  []           = error "computeAlt: no equations"
computeAlt pat pats er prps@(p : _) = do
  -- oldpats need to be computed for each pattern
  (capp, nvars, _) <- decomposeConPat (firstPat p)
  nprps            <- mapM f prps
  let sub = tSubst (translatePVar pat) (translateApp capp)
  res <- match (nvars ++ pats) nprps (substitute sub er)
  let res' = substitute sub res
  return (S.alt capp res')
 where
  f :: Member Fresh r => Eqs a -> Sem r (Eqs a)
  f ([]    , r) = return ([], r) -- potentially unused
  f (v : vs, r) = do
    (_, _, oldpats) <- decomposeConPat v
    return (oldpats ++ vs, r)

-- | Converts a constructor application pattern (where the argument patterns
--   are variable or wildcard patterns) to an expression.
translateApp :: S.Pat a -> S.Exp a
translateApp (S.PApp _ qn ps) = foldl
  (\acc x -> S.App S.NoSrcSpan acc (translatePVar x))
  (S.Con S.NoSrcSpan qn)
  ps
translateApp (S.PInfixApp _ p1 qn p2) = S.InfixApp S.NoSrcSpan
                                                   (translatePVar p1)
                                                   (S.QConOp S.NoSrcSpan qn)
                                                   (translatePVar p2)
translateApp (S.PTuple _ bxd ps) =
  S.Tuple S.NoSrcSpan bxd $ map translatePVar ps
translateApp (S.PList _ ps) = S.List S.NoSrcSpan $ map translatePVar ps
translateApp _              = error "translateApp: Unsupported pattern"
          -- pat = error ("translateApp does not support: " ++ show pat)

-- TODO refactor into 2 functions. one for the capp and nvars and one for the
--      oldpats

-- | Replaces the child patterns of a constructor pattern with fresh variable
--   patterns.
--
--   Returns the constructor with replaced child patterns and a list of
--   new and old variable patterns.
decomposeConPat
  :: Member Fresh r => S.Pat a -> Sem r (S.Pat a, [S.Pat a], [S.Pat a])
decomposeConPat (S.PApp _ qname ps) = do
  nvars <- replicateM (length ps) (freshVarPat genericFreshPrefix)
  return (S.PApp S.NoSrcSpan qname nvars, nvars, ps)
decomposeConPat (S.PInfixApp _ p1 qname p2) = do
  nvars <- replicateM 2 (freshVarPat genericFreshPrefix)
  let [nv1, nv2] = nvars
      ps         = [p1, p2]
  return (S.PInfixApp S.NoSrcSpan nv1 qname nv2, nvars, ps)
decomposeConPat (S.PParen _ p) = decomposeConPat p
decomposeConPat (S.PList _ ps)
  | null ps = return (S.PList S.NoSrcSpan [], [], [])
  | otherwise = do
    let (n : nv) = ps
        listCon  = S.Special S.NoSrcSpan $ S.Cons S.NoSrcSpan
    decomposeConPat (S.PInfixApp S.NoSrcSpan n listCon (S.PList S.NoSrcSpan nv))
decomposeConPat (S.PTuple _ bxd ps) = do
  nvars <- replicateM (length ps) (freshVarPat genericFreshPrefix)
  return (S.PTuple S.NoSrcSpan bxd nvars, nvars, ps)
-- wildcards no longer needed as cons
decomposeConPat (S.PWildCard _) = return (S.PWildCard S.NoSrcSpan, [], [])
decomposeConPat _               = error "wrong Pattern in decomposeConPat"

-- | Tests whether the pattern lists of all given equations starts with a
--   variable pattern.
allVars :: [Eqs a] -> Bool
allVars = all (isVar . firstPat)

-- | Gets the first pattern of the pattern list of the given equation.
firstPat :: Eqs a -> S.Pat a
firstPat = head . fst

-- | Tests whether the given pattern is a variable or wildcard pattern.
isVar :: S.Pat a -> Bool
isVar (S.PVar _ _   ) = True
isVar (S.PWildCard _) = True
isVar _               = False

-- | Tests whether the pattern lists of all given equations starts with a
--   constructor pattern.
allCons :: [Eqs a] -> Bool
allCons = all (isCons . firstPat)

-- | Tests whether the given pattern is a constructor pattern.
--
--   Special patterns for lists and tuples are also considered constructor
--   patterns.
isCons :: S.Pat a -> Bool
isCons p = case p of
  S.PApp _ _ _        -> True
  S.PInfixApp _ _ _ _ -> True
  S.PParen _ p'       -> isCons p'
  S.PList  _ _        -> True
  S.PTuple _ _ _      -> True
  S.PWildCard _       -> False -- Wildcards are now treated as variables
  _                   -> False
