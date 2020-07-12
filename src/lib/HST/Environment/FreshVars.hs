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
type Constructor a = (S.QName a, Int, Bool)

getConstrArity :: Constructor a -> Int
getConstrArity (_, a, _) = a

getConstrName :: Constructor a -> S.QName a
getConstrName (n, _, _) = n

isInfixConst :: Constructor a -> Bool
isInfixConst (_, _, b) = b

data PMState a = PMState
  { nextId      :: Int
  , constrMap   :: [(String, [Constructor a])] -- Arity
  , matchedPat  :: [(S.Exp a, S.Pat a)] -- Variable and binded Cons
  , trivialCC   :: Bool
  , opt         :: Bool -- optimize case exps
  }

newtype PM a b = PM { unwrapPM :: State (PMState a) b }
 deriving (Functor, Applicative, Monad, MonadState (PMState a))

runPM :: PM a b -> PMState a -> (b, PMState a)
runPM = State.runState . unwrapPM

evalPM :: PM a b -> PMState a -> b
evalPM = State.evalState . unwrapPM

instance MonadFail (PM a) where
  fail = error

freshVar :: PM a Int
freshVar = do
  i <- State.gets nextId
  State.modify $ \state -> state { nextId = i + 1 }
  return i

-- | Generates the given number of fresh variables.
--
--   The generated variables use IDs from the state.
newVars :: Int -> PM a [S.Pat a]
newVars 0 = return []
newVars n = do
  nvar <- newVar
  vs   <- newVars (n - 1)
  return (nvar : vs)

-- | Generates a single fresh variable with an ID from the state.
newVar :: PM a (S.Pat a)
newVar = do
  nv <- freshVar
  let v = 'a' : show nv
  return (S.PVar S.NoSrcSpan (S.Ident S.NoSrcSpan v))

addConstrMap :: (String, [Constructor a]) -> PM a ()
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
