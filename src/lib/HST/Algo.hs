-- | This module contains the actual implementation of the pattern-matching
--   compilation algorithm.

module HST.Algo
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

import qualified Language.Haskell.Exts.Syntax  as HSE
import qualified Language.Haskell.Exts.Build   as B
import qualified Language.Haskell.Exts.Pretty  as P


-- | A type that represents a single equation of a function declaration.
--
--   An equation is characterized by the argument patterns and the right-hand
--   side.
type Eqs = ([HSE.Pat ()], HSE.Exp ())

-- | The default error expression to insert for pattern matching failures.
err :: HSE.Exp ()
err = HSE.Var () (HSE.UnQual () (HSE.Ident () "undefined"))

-- | Compiles the given equations of a function declaration to a single
--   expression that performs explicit pattern matching using @case@
--   expressions.
--
--   All equations must have the same number of patterns as the given list
--   of fresh variable patterns.
match
  :: [HSE.Pat ()] -- ^ Fresh variable patterns.
  -> [Eqs]    --     ^ The equations of the function declaration.
  -> HSE.Exp ()   -- ^ The error expression for pattern-matching failures.
  -> PM (HSE.Exp ())
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
substVars :: HSE.Pat () -> Eqs -> PM Eqs
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
getPVarName :: HSE.Pat () -> PM String
getPVarName (HSE.PVar _ pname) = getNameStr pname
 where
  getNameStr (HSE.Ident  _ str) = return str
  getNameStr (HSE.Symbol _ str) = return str
getPVarName (HSE.PWildCard _) = do
  n <- freshVar
  return $ 'a' : show n
getPVarName x =
  error $ "getPVarName: expected variable or wildcard pattern, got " ++ show x

-- | Groups the given equations based on the type of their first pattern.
--
--   The order of the equations is not changed, i.e., concatenation of the
--   resulting groups yields the original list of equations.

--   Two equations are in the same group only if their pattern lists both
--   start with a variable pattern or both start with a constructor pattern.
groupPat :: [Eqs] -> [[Eqs]]
groupPat = groupBy ordPats
  where ordPats (ps, _) (qs, _) = isPVar (head ps) && isPVar (head qs)

-- | Tests whether the given pattern is a variable pattern.
--
--   TODO shouldn't this return @True@ for wildcard patterns, too?
--   What's the difference to @isVar@?
isPVar :: HSE.Pat () -> Bool
isPVar (HSE.PVar _ _) = True
isPVar _              = False

-- | Applies 'match' to every group of equations where the error expression
--   is the 'match' result of the next group.
createRekMatch
  :: [HSE.Pat ()] -- ^ Fresh variable patterns.
  -> HSE.Exp ()   -- ^ The error expression for pattern-matching failures.
  -> [[Eqs]]      -- ^ Groups of equations (see 'groupPat').
  -> PM (HSE.Exp ())
createRekMatch vars er =
  foldr (\eqs mrhs -> mrhs >>= match vars eqs) (return er)

-- | Creates a case expression that performs pattern matching on the variable
--   bound by the given variable pattern.
makeRhs
  :: HSE.Pat ()   -- ^ The fresh variable pattern to match.
  -> [HSE.Pat ()] -- ^ The remaing fresh variable patterns.
  -> [Eqs]        -- ^ The equations.
  -> HSE.Exp ()   -- ^ The error expression for pattern-matching failures.
  -> PM (HSE.Exp ())
makeRhs x xs eqs er = do
  alts <- computeAlts x xs eqs er
  return (B.caseE (translatePVar x) alts)

-- | Converts the given variable pattern to a variable expression.
translatePVar :: HSE.Pat () -> HSE.Exp ()
translatePVar (HSE.PVar _ vname) = B.var vname
translatePVar p =
  error ("translatePVar: expected variable pattern, got" ++ show p)

-- TODO remove redundand types

-- | Generates @case@ expression alternatives for the given equations.
--
--   If there are missing constructors and trivial case completion is enabled,
--   an alternative with an wildcard pattern is added.
--   If trivial case completion is not enabled, one alternative is added for
--   every missing constructor.
computeAlts
  :: HSE.Pat ()   -- ^ The variable pattern that binds the matched variable.
  -> [HSE.Pat ()] -- ^ The remaing fresh variable patterns.
  -> [Eqs]        -- ^ The equations to generate alternatives for.
  -> HSE.Exp ()   -- ^ The error expression for pattern-matching failures.
  -> PM [HSE.Alt ()]
computeAlts x xs eqs er = do
  alts <- mapM (computeAlt x xs er) (groupByCons eqs)
  mxs  <- getMissingConstrs alts
  case mxs of
    [] -> return alts
    zs -> do
      b <- gets trivialCC
      if b
        then -- TODO is 'err' correct? Why not 'er'?
             return $ alts ++ [B.alt B.wildcard err]
        else do
          z <- createAltsFromConstr x zs er
          -- TODO currently not sorted (reversed)
          return $ alts ++ z

-- | Looks up the constructors of the data type that is matched by the given
--   @case@ expression alternatives for which there are no alternatives already.
getMissingConstrs :: [HSE.Alt ()] -> PM [Constructor]
getMissingConstrs []   = error "getMissingConstrs: empty list"
getMissingConstrs alts = do
  cmap <- gets constrMap -- [(datantype, (constructor,arity))]
  let names     = map getQName alts -- Names of matched constructors.
      (_, cons) = findDataType (head names) cmap
  return (findCons cons names)

-- | Removes the 'Constructor's with the given names from the given list.
findCons
  :: [Constructor]  -- TODO rename // complement
  -> [HSE.QName ()]
  -> [Constructor]
findCons cons usedcons =
  filter (\con -> getConstrName con `notElem` usedcons) cons

-- | Looks up the data type for the given constructor in the given map.
findDataType
  :: HSE.QName ()                   -- Constructor name
  -> [(String, [Constructor])]  -- [(Datatype, [(Konstruktor,Arität)])]
  -> (String, [Constructor])     -- Datatype
findDataType cname = foldr
  (\c sc -> if cname `elem` map getConstrName (snd c) then c else sc)
  (error
    ("findDataType: could not find data type for constructor " ++ show cname)
  ) -- todo fst und snd als synonym

-- | Gets the name of the constructor matched by the given @case@ expression
--   alternative.
getQName :: HSE.Alt () -> HSE.QName ()
getQName (HSE.Alt _ p _ _) = getQNamePat p
-- getQName  _             = error "getQName: expected an Alt"

-- | Gets the name of a constructor pattern.
--
--   Returns the 'HSE.Special' names for special patterns such as lists and tuples.
getQNamePat :: HSE.Pat () -> HSE.QName ()
getQNamePat (HSE.PApp _ qn _       ) = qn
getQNamePat (HSE.PInfixApp _ _ qn _) = qn
getQNamePat (HSE.PList _ _         ) = HSE.Special () (HSE.ListCon ())
getQNamePat (HSE.PWildCard _       ) = HSE.Special () (HSE.ExprHole ()) -- TODO aren't wildcard pattern considers variable and not constructor patterns?
getQNamePat (HSE.PTuple _ bxd ps) =
  HSE.Special () (HSE.TupleCon () bxd (length ps))
getQNamePat _ = error "getQNamePat unsuported Pattern"

-- TODO refactor with smartcons

-- | Creates new @case@ expression alternatives for the given missing
--   constructors.
createAltsFromConstr
  :: HSE.Pat ()        -- ^ The fresh variable matched by the @case@ expression.
  -> [Constructor]     -- ^ The missing constructors to generate alternatives for.
  -> HSE.Exp ()        -- ^ The error expression for pattern-matching failures.
  -> PM [HSE.Alt ()]
createAltsFromConstr x cs er = mapM (createAltFromConstr x er) cs
 where
  createAltFromConstr
    :: HSE.Pat () -> HSE.Exp () -> Constructor -> PM (HSE.Alt ())
  createAltFromConstr pat e (qn, ar, b) = do
    nvars <- newVars ar
    let p | b         = HSE.PInfixApp () (head nvars) qn (nvars !! 1)
          | otherwise = HSE.PApp () qn nvars
        p'   = translateApp p
        pat' = translatePVar pat
        e'   = substitute (tSubst pat' p') e
    return (B.alt p e')

-- | Groups the given equations based on the constructor matched by their
--   first pattern.
--
--   The order of equations that match the same constructor (i.e., within
--   groups) is preserved.
groupByCons :: [Eqs] -> [[Eqs]]
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
select :: Eqs -> Eqs -> Bool
select (ps, _) (qs, _) = compareCons (head ps) (head qs)

-- | Tests whether the given patterns match the same constructor or both match
--   the wildcard pattern.
compareCons :: HSE.Pat () -> HSE.Pat () -> Bool
compareCons = (==) `on` consName

-- | Returns the qualified name of the constructor of the given pattern or
--   @Nothing@ if it is a wildcard pattern.
consName :: HSE.Pat () -> Maybe (HSE.QName ())
consName (HSE.PApp _ qn _       ) = return qn
consName (HSE.PInfixApp _ _ qn _) = return qn
consName (HSE.PParen _ pat      ) = consName pat
consName (HSE.PList  _ []       ) = return $ HSE.Special () $ HSE.ListCon ()
consName (HSE.PList  _ (_ : _)  ) = return $ HSE.Special () $ HSE.Cons ()
consName (HSE.PTuple _ bxd ps) =
  return $ HSE.Special () $ HSE.TupleCon () bxd $ length ps
consName (HSE.PWildCard _) = Nothing
consName pat =
  error $ "consName: unsupported pattern \"" ++ P.prettyPrint pat ++ "\""

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
  :: HSE.Pat ()   -- ^ The variable pattern that binds the matched variable.
  -> [HSE.Pat ()] -- ^ The remaing fresh variable patterns.
  -> HSE.Exp ()   -- ^ The error expression for pattern-matching failures.
  -> [Eqs]        -- ^ A group of equations (see 'groupByCons').
  -> PM (HSE.Alt ())
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
  f :: Eqs -> PM Eqs
  f ([]    , r) = return ([], r) -- potentially unused
  f (v : vs, r) = do
    (_, _, oldpats) <- getConst v
    return (oldpats ++ vs, r)

-- | Converts a constructor application pattern (where the argument patterns
--   are variable or wildcard patterns) to an expression.
translateApp :: HSE.Pat () -> HSE.Exp ()
translateApp (HSE.PApp _ qn ps) =
  foldl (\acc x -> HSE.App () acc (translatePVar x)) (HSE.Con () qn) ps
translateApp (HSE.PInfixApp _ p1 qn p2) =
  HSE.InfixApp () (translatePVar p1) (HSE.QConOp () qn) (translatePVar p2)
translateApp (HSE.PTuple _ bxd ps) = HSE.Tuple () bxd $ map translatePVar ps
translateApp (HSE.PList _ ps) = HSE.List () $ map translatePVar ps
translateApp pat = error ("translateApp does not support: " ++ show pat)

-- TODO refactor into 2 functions. one for the capp and nvars and one for the
-- oldpats

-- | Replaces the child patterns of a constructor pattern with fresh variable
--   patterns.
--
--   Returns the constructor with replaced child patterns and a list of
--   new and old variable patterns.
getConst :: HSE.Pat () -> PM (HSE.Pat (), [HSE.Pat ()], [HSE.Pat ()])
getConst (HSE.PApp _ qname ps) = do
  nvars <- newVars (length ps)
  return (HSE.PApp () qname nvars, nvars, ps)
getConst (HSE.PInfixApp _ p1 qname p2) = do
  nvars <- newVars 2
  let [nv1, nv2] = nvars
      ps         = [p1, p2]
  return (HSE.PInfixApp () nv1 qname nv2, nvars, ps)
getConst (HSE.PParen _ p) = getConst p
getConst (HSE.PList _ ps)
  | null ps = return (HSE.PList () [], [], [])
  | otherwise = do
    let (n : nv) = ps
        listCon  = HSE.Special () $ HSE.Cons ()
    getConst (HSE.PInfixApp () n listCon (HSE.PList () nv))
getConst (HSE.PTuple _ bxd ps) = do
  nvars <- newVars (length ps)
  return (HSE.PTuple () bxd nvars, nvars, ps)
-- wildcards no longer needed as cons
getConst (HSE.PWildCard _) = return (HSE.PWildCard (), [], [])
getConst _                 = error "wrong Pattern in getConst"

-- | Tests whether the pattern lists of all given equations starts with a
--   variable pattern.
allVars :: [Eqs] -> Bool
allVars = all (isVar . firstPat)

-- | Gets the first pattern of the pattern list of the given equation.
firstPat :: Eqs -> HSE.Pat ()
firstPat = head . fst

-- | Tests whether the given pattern is a variable or wildcard pattern.
isVar :: HSE.Pat () -> Bool
isVar (HSE.PVar _ _   ) = True
isVar (HSE.PWildCard _) = True
isVar _                 = False

-- | Tests whether the pattern lists of all given equations starts with a
--   constructor pattern.
allCons :: [Eqs] -> Bool
allCons = all (isCons . firstPat)

-- | Tests whether the given pattern is a constructor pattern.
--
--   Special patterns for lists and tuples are also considered constructor
--   patterns.
isCons :: HSE.Pat () -> Bool
isCons p = case p of
  HSE.PApp _ _ _        -> True
  HSE.PInfixApp _ _ _ _ -> True
  HSE.PParen _ p'       -> isCons p'
  HSE.PList  _ _        -> True
  HSE.PTuple _ _ _      -> True
  HSE.PWildCard _       -> False -- Wildcards are now treated as variables
  _                     -> False
