{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

import qualified Language.Haskell.Exts.Syntax  as S
import qualified Language.Haskell.Exts.Build   as B

-- QName instead of String to support special Syntax
-- Bool isInfix
type Constructor = (S.QName (), Int, Bool)

getConstrArity :: Constructor -> Int
getConstrArity (_, a, _) = a

getConstrName :: Constructor -> S.QName ()
getConstrName (n, _, _) = n

isInfixConst :: Constructor -> Bool
isInfixConst (_, _, b) = b

data PMState = PMState
  { nextId      :: Int
  , constrMap   :: [(String, [Constructor])] -- Arity
  , matchedPat  :: [(S.Exp (), S.Pat () )] -- Variable and binded Cons
  , trivialCC   :: Bool
  , opt         :: Bool -- optimize case exps
  , debugOutput :: String
  }

newtype PM a = PM { unwrapPM :: State PMState a }
 deriving (Functor, Applicative, Monad, MonadState PMState)

runPM :: PM a -> PMState -> (a, PMState)
runPM = State.runState . unwrapPM

evalPM :: PM a -> PMState -> a
evalPM = State.evalState . unwrapPM

instance MonadFail PM where
  fail = error

freshVar :: PM Int
freshVar = do
  i <- State.gets nextId
  State.modify $ \state -> state { nextId = i + 1 }
  --debug <- gets debugOutput
  --modify $ \state -> state {debugOutput = "Generated"++ show i ++", "++debug}
  return i

-- | Generates the given number of fresh variables.
--
--   The generated variables use IDs from the state.
newVars :: Int -> PM [S.Pat ()]
newVars 0 = return []
newVars n = do
  nvar <- newVar
  vs   <- newVars (n - 1)
  return (nvar : vs)

-- | Generates a single fresh variable with an ID from the state.
newVar :: PM (S.Pat ())
newVar = do
  nv <- freshVar
  let v = 'a' : show nv
  return (B.pvar (B.name v))

addConstrMap :: (String, [Constructor]) -> PM ()
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

addDebug :: String -> PM ()
addDebug s = do
  debug <- State.gets debugOutput
  State.modify $ \state -> state { debugOutput = s ++ debug }
