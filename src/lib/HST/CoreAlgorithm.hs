-- | This module contains the actual implementation of the pattern-matching
--   compilation algorithm.

module HST.CoreAlgorithm
  ( match
  , err
  , translatePVar
  , Eqs
  , isPVar
  , isCons
  , getPVarName
  , getQNamePat
  -- * Testing interface
  , compareCons
  )
where

import           Data.List                      ( partition
                                                , groupBy
                                                )
import           Data.Function                  ( on )
import           HST.Environment.FreshVars      ( PM
                                                , Constructor
                                                , getConstrName
                                                , constrMap
                                                , trivialCC
                                                , freshVar
                                                , newVars
                                                , gets
                                                )
import           HST.Environment.Renaming       ( subst
                                                , tSubst
                                                , rename
                                                , substitute
                                                )
import qualified HST.Frontend.Syntax           as S
import qualified HST.Frontend.Build            as B
--import qualified Language.Haskell.Exts.Pretty  as P


-- | A type that represents a single equation of a function declaration.
--
--   An equation is characterized by the argument patterns and the right-hand
--   side.
type Eqs s l t = ([S.Pat s l], S.Exp s l t)

-- | The default error expression to insert for pattern matching failures.
err :: S.Exp s l t
err =
  S.Var S.NoSrcSpan (S.UnQual S.NoSrcSpan (S.Ident S.NoSrcSpan "undefined"))

-- | Compiles the given equations of a function declaration to a single
--   expression that performs explicit pattern matching using @case@
--   expressions.
--
--   All equations must have the same number of patterns as the given list
--   of fresh variable patterns.
match
  :: (Eq l, Eq t)
  => [S.Pat s l] -- ^ Fresh variable patterns.
  -> [Eqs s l t] -- ^ The equations of the function declaration.
  -> S.Exp s l t -- ^ The error expression for pattern-matching failures.
  -> PM s l t (S.Exp s l t)
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
    -- unsupported patterns.
    otherwise = createRekMatch vars er (groupPat eqs)
match [] _ _ = error "match: equations have different number of arguments"

-- | Substitutes all occurrences of the variable bound by the first pattern
--   (must be a variable or wildcard pattern) of the given equation by the
--   fresh variable bound by the given variable pattern on the right-hand side
--   of the equation.
substVars :: S.Pat s l -> Eqs s l t -> PM s l t (Eqs s l t)
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
getPVarName :: S.Pat s l -> PM s l t String
getPVarName (S.PVar _ pname) = getNameStr pname
 where
  getNameStr (S.Ident  _ str) = return str
  getNameStr (S.Symbol _ str) = return str
getPVarName (S.PWildCard _) = do
  n <- freshVar
  return $ 'a' : show n
getPVarName _ = error "getPVarName: expected variable or wildcard pattern" --, got " ++ show x

-- | Groups the given equations based on the type of their first pattern.
--
--   The order of the equations is not changed, i.e., concatenation of the
--   resulting groups yields the original list of equations.

--   Two equations are in the same group only if their pattern lists both
--   start with a variable pattern or both start with a constructor pattern.
groupPat :: [Eqs s l t] -> [[Eqs s l t]]
groupPat = groupBy ordPats
  where ordPats (ps, _) (qs, _) = isPVar (head ps) && isPVar (head qs)

-- | Tests whether the given pattern is a variable pattern.
--
--   TODO shouldn't this return @True@ for wildcard patterns, too?
--   What's the difference to @isVar@?
isPVar :: S.Pat s l -> Bool
isPVar (S.PVar _ _) = True
isPVar _            = False

-- | Applies 'match' to every group of equations where the error expression
--   is the 'match' result of the next group.
createRekMatch
  :: (Eq l, Eq t)
  => [S.Pat s l]   -- ^ Fresh variable patterns.
  -> S.Exp s l t   -- ^ The error expression for pattern-matching failures.
  -> [[Eqs s l t]] -- ^ Groups of equations (see 'groupPat').
  -> PM s l t (S.Exp s l t)
createRekMatch vars er =
  foldr (\eqs mrhs -> mrhs >>= match vars eqs) (return er)

-- | Creates a case expression that performs pattern matching on the variable
--   bound by the given variable pattern.
makeRhs
  :: (Eq l, Eq t)
  => S.Pat s l   -- ^ The fresh variable pattern to match.
  -> [S.Pat s l] -- ^ The remaing fresh variable patterns.
  -> [Eqs s l t] -- ^ The equations.
  -> S.Exp s l t -- ^ The error expression for pattern-matching failures.
  -> PM s l t (S.Exp s l t)
makeRhs x xs eqs er = do
  alts <- computeAlts x xs eqs er
  return (S.Case S.NoSrcSpan (translatePVar x) alts)

-- | Converts the given variable pattern to a variable expression.
translatePVar :: S.Pat s l -> S.Exp s l t
translatePVar (S.PVar _ vname) = B.var vname
translatePVar _ = error "translatePVar: expected variable pattern" --, got" ++ show p)

-- TODO remove redundand types

-- | Generates @case@ expression alternatives for the given equations.
--
--   If there are missing constructors and trivial case completion is enabled,
--   an alternative with an wildcard pattern is added.
--   If trivial case completion is not enabled, one alternative is added for
--   every missing constructor.
computeAlts
  :: (Eq l, Eq t)
  => S.Pat s l   -- ^ The variable pattern that binds the matched variable.
  -> [S.Pat s l] -- ^ The remaing fresh variable patterns.
  -> [Eqs s l t] -- ^ The equations to generate alternatives for.
  -> S.Exp s l t -- ^ The error expression for pattern-matching failures.
  -> PM s l t [S.Alt s l t]
computeAlts x xs eqs er = do
  alts <- mapM (computeAlt x xs er) (groupByCons eqs)
  mxs  <- getMissingConstrs alts
  case mxs of
    [] -> return alts
    zs -> do
      b <- gets trivialCC
      if b
        then -- TODO is 'err' correct? Why not 'er'?
             return $ alts ++ [B.alt (S.PWildCard S.NoSrcSpan) err]
        else do
          z <- createAltsFromConstr x zs er
          -- TODO currently not sorted (reversed)
          return $ alts ++ z

-- | Looks up the constructors of the data type that is matched by the given
--   @case@ expression alternatives for which there are no alternatives already.
getMissingConstrs :: [S.Alt s l t] -> PM s l t [Constructor s]
getMissingConstrs []   = error "getMissingConstrs: empty list"
getMissingConstrs alts = do
  cmap <- gets constrMap -- [(datantype, (constructor,arity))]
  let names     = map getQName alts -- Names of matched constructors.
      (_, cons) = findDataType (head names) cmap
  return (findCons cons names)

-- | Removes the 'Constructor's with the given names from the given list.
findCons
  :: [Constructor s]  -- TODO rename // complement
  -> [S.QName s]
  -> [Constructor s]
findCons cons usedcons =
  filter (\con -> getConstrName con `notElem` usedcons) cons

-- | Looks up the data type for the given constructor in the given map.
findDataType
  :: S.QName s                   -- Constructor name
  -> [(String, [Constructor s])] -- [(Datatype, [(Konstruktor,Arität)])]
  -> (String, [Constructor s])   -- Datatype
findDataType cname = foldr
  (\c sc -> if cname `elem` map getConstrName (snd c) then c else sc)
  (error "findDataType: could not find data type for constructor" -- " ++ show cname)
                                                                 ) -- todo fst und snd als synonym

-- | Gets the name of the constructor matched by the given @case@ expression
--   alternative.
getQName :: S.Alt s l t -> S.QName s
getQName (S.Alt _ p _ _) = getQNamePat p
-- getQName  _             = error "getQName: expected an Alt"

-- | Gets the name of a constructor pattern.
--
--   Returns the 'S.Special' names for special patterns such as lists and tuples.
getQNamePat :: S.Pat s l -> S.QName s
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
  :: (Eq l, Eq t)
  => S.Pat s l       -- ^ The fresh variable matched by the @case@ expression.
  -> [Constructor s] -- ^ The missing constructors to generate alternatives for.
  -> S.Exp s l t     -- ^ The error expression for pattern-matching failures.
  -> PM s l t [S.Alt s l t]
createAltsFromConstr x cs er = mapM (createAltFromConstr x er) cs
 where
  createAltFromConstr
    :: (Eq l, Eq t)
    => S.Pat s l
    -> S.Exp s l t
    -> Constructor s
    -> PM s l t (S.Alt s l t)
  createAltFromConstr pat e (qn, ar, b) = do
    nvars <- newVars ar
    let p | b         = S.PInfixApp S.NoSrcSpan (head nvars) qn (nvars !! 1)
          | otherwise = S.PApp S.NoSrcSpan qn nvars
        p'   = translateApp p
        pat' = translatePVar pat
        e'   = substitute (tSubst pat' p') e
    return (B.alt p e')

-- | Groups the given equations based on the constructor matched by their
--   first pattern.
--
--   The order of equations that match the same constructor (i.e., within
--   groups) is preserved.
groupByCons :: [Eqs s l t] -> [[Eqs s l t]]
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
select :: Eqs s l t -> Eqs s l t -> Bool
select (ps, _) (qs, _) = compareCons (head ps) (head qs)

-- | Tests whether the given patterns match the same constructor or both match
--   the wildcard pattern.
compareCons :: S.Pat s l -> S.Pat s l -> Bool
compareCons = (==) `on` consName

-- | Returns the qualified name of the constructor of the given pattern or
--   @Nothing@ if it is a wildcard pattern.
consName :: S.Pat s l -> Maybe (S.QName s)
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
consName _               = error $ "consName: unsupported pattern" -- \"" ++ P.prettyPrint pat ++ "\""

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
  :: (Eq l, Eq t)
  => S.Pat s l   -- ^ The variable pattern that binds the matched variable.
  -> [S.Pat s l] -- ^ The remaing fresh variable patterns.
  -> S.Exp s l t -- ^ The error expression for pattern-matching failures.
  -> [Eqs s l t] -- ^ A group of equations (see 'groupByCons').
  -> PM s l t (S.Alt s l t)
computeAlt _   _    _  []           = error "computeAlt: no equations"
computeAlt pat pats er prps@(p : _) = do
  -- oldpats need to be computed for each pattern
  (capp, nvars, _) <- getConst (firstPat p)
  nprps            <- mapM f prps
  let sub = tSubst (translatePVar pat) (translateApp capp)
  res <- match (nvars ++ pats) nprps (substitute sub er)
  let res' = substitute sub res
  return (B.alt capp res')
 where
  f :: Eqs s l t -> PM s l t (Eqs s l t)
  f ([]    , r) = return ([], r) -- potentially unused
  f (v : vs, r) = do
    (_, _, oldpats) <- getConst v
    return (oldpats ++ vs, r)

-- | Converts a constructor application pattern (where the argument patterns
--   are variable or wildcard patterns) to an expression.
translateApp :: S.Pat s l -> S.Exp s l t
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
-- oldpats

-- | Replaces the child patterns of a constructor pattern with fresh variable
--   patterns.
--
--   Returns the constructor with replaced child patterns and a list of
--   new and old variable patterns.
getConst :: S.Pat s l -> PM s l t (S.Pat s l, [S.Pat s l], [S.Pat s l])
getConst (S.PApp _ qname ps) = do
  nvars <- newVars (length ps)
  return (S.PApp S.NoSrcSpan qname nvars, nvars, ps)
getConst (S.PInfixApp _ p1 qname p2) = do
  nvars <- newVars 2
  let [nv1, nv2] = nvars
      ps         = [p1, p2]
  return (S.PInfixApp S.NoSrcSpan nv1 qname nv2, nvars, ps)
getConst (S.PParen _ p) = getConst p
getConst (S.PList _ ps)
  | null ps = return (S.PList S.NoSrcSpan [], [], [])
  | otherwise = do
    let (n : nv) = ps
        listCon  = S.Special S.NoSrcSpan $ S.Cons S.NoSrcSpan
    getConst (S.PInfixApp S.NoSrcSpan n listCon (S.PList S.NoSrcSpan nv))
getConst (S.PTuple _ bxd ps) = do
  nvars <- newVars (length ps)
  return (S.PTuple S.NoSrcSpan bxd nvars, nvars, ps)
-- wildcards no longer needed as cons
getConst (S.PWildCard _) = return (S.PWildCard S.NoSrcSpan, [], [])
getConst _               = error "wrong Pattern in getConst"

-- | Tests whether the pattern lists of all given equations starts with a
--   variable pattern.
allVars :: [Eqs s l t] -> Bool
allVars = all (isVar . firstPat)

-- | Gets the first pattern of the pattern list of the given equation.
firstPat :: Eqs s l t -> S.Pat s l
firstPat = head . fst

-- | Tests whether the given pattern is a variable or wildcard pattern.
isVar :: S.Pat s l -> Bool
isVar (S.PVar _ _   ) = True
isVar (S.PWildCard _) = True
isVar _               = False

-- | Tests whether the pattern lists of all given equations starts with a
--   constructor pattern.
allCons :: [Eqs s l t] -> Bool
allCons = all (isCons . firstPat)

-- | Tests whether the given pattern is a constructor pattern.
--
--   Special patterns for lists and tuples are also considered constructor
--   patterns.
isCons :: S.Pat s l -> Bool
isCons p = case p of
  S.PApp _ _ _        -> True
  S.PInfixApp _ _ _ _ -> True
  S.PParen _ p'       -> isCons p'
  S.PList  _ _        -> True
  S.PTuple _ _ _      -> True
  S.PWildCard _       -> False -- Wildcards are now treated as variables
  _                   -> False
