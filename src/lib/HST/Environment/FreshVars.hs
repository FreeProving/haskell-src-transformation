{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This module contains methods for generating fresh variable identifiers.
--   It also contains the @Constructor@ type, including accessing functions,
--   and the @PMState@ data type, including run and evaluation functions.

module HST.Environment.FreshVars
  ( module HST.Environment.FreshVars
  , State.gets
  , State.modify
  )
where

import           Prelude                 hiding ( fail )

import           Control.Monad.Fail             ( MonadFail(..) )
import           Control.Monad.State            ( State
                                                , MonadState
                                                )
import qualified Control.Monad.State           as State

import qualified HST.Frontend.Syntax           as S

-- QName instead of String to support special Syntax
-- Bool isInfix
type Constructor s = (S.QName s, Int, Bool)

getConstrArity :: Constructor s -> Int
getConstrArity (_, a, _) = a

getConstrName :: Constructor s -> S.QName s
getConstrName (n, _, _) = n

isInfixConst :: Constructor s -> Bool
isInfixConst (_, _, b) = b

data PMState s l t = PMState
  { nextId      :: Int
  , constrMap   :: [(String, [Constructor s])] -- Arity
  , matchedPat  :: [(S.Exp s l t, S.Pat s l)] -- Variable and binded Cons
  , trivialCC   :: Bool
  , opt         :: Bool -- optimize case exps
  , debugOutput :: String
  }

newtype PM s l t a = PM { unwrapPM :: State (PMState s l t) a }
 deriving (Functor, Applicative, Monad, MonadState (PMState s l t))

runPM :: PM s l t a -> PMState s l t -> (a, PMState s l t)
runPM = State.runState . unwrapPM

evalPM :: PM s l t a -> PMState s l t -> a
evalPM = State.evalState . unwrapPM

instance MonadFail (PM s l t) where
  fail = error

freshVar :: PM s l t Int
freshVar = do
  i <- State.gets nextId
  State.modify $ \state -> state { nextId = i + 1 }
  --debug <- gets debugOutput
  --modify $ \state -> state {debugOutput = "Generated"++ show i ++", "++debug}
  return i

-- | Generates the given number of fresh variables.
--
--   The generated variables use IDs from the state.
newVars :: Int -> PM s l t [S.Pat s l]
newVars 0 = return []
newVars n = do
  nvar <- newVar
  vs   <- newVars (n - 1)
  return (nvar : vs)

-- | Generates a single fresh variable with an ID from the state.
newVar :: PM s l t (S.Pat s l)
newVar = do
  nv <- freshVar
  let v = 'a' : show nv
  return (S.PVar S.NoSrcSpan (S.Ident S.NoSrcSpan v))

addConstrMap :: (String, [Constructor s]) -> PM s l t ()
addConstrMap cs = do
  cmap <- State.gets constrMap
  State.modify $ \state -> state { constrMap = cs : cmap }
{-

renameFunc :: FuncDecl -> FreshVar FuncDecl
renameFunc (Func ...) = do
  i <- freshVar
  return (FuncDecl ...)

processProg :: Prog -> Prog
processProg p = evalPM (renameProg p) 0

-}

addDebug :: String -> PM s l t ()
addDebug s = do
  debug <- State.gets debugOutput
  State.modify $ \state -> state { debugOutput = s ++ debug }
