module Monad.Monads5 where

import Control.Monad.State (State)
import qualified Control.Monad.State as S
import Monad.Ops (IntAdd (..), Op (..))
import qualified Monad.Ops as O

{-

- The *State* monad combines the functionality of the Reader and Writer monads.
  We have a single stateful object, and we are free to access and read from it,
  or update and change its values. When we change the object, subsequent operations
  in the monad will refer to the updated value.

instance Monad (State s) where
  ...

- Note the state does NOT have to be a Monoid, as with Writer

- We retrieve the state with the 'get' function. We can replace the state with
  a new object by using 'put':

get :: State s s
put :: s -> State s ()

- The 'runState' function works like 'runReader' and 'runWriter'. It requires
  an initial state parameter and produces both the computation result and the
  final state.

runState :: State s a -> s -> (a, s)

- If you only care about the final computation result, you can use
  'evalState' instead of 'runState'. If you only care about the final
  state, you can use 'execState':

evalState :: State s a -> s -> a
execState :: State s a -> s -> s

- There are a couple other functions you can use. Just like we have
  'asks' in Reader, there is 'gets' which can retrieve a field from the State.

gets :: (s -> a) -> State s a

- Then you can use 'modify' to apply a function on the state:

modify :: (s -> s) -> State s ()

runState (modify (+4) 5) -> ((), 9)

-}

-- TODO:

-- Rewrite these functions from last time, but use the State monad
-- instead of the Writer monad! Notice however, that you do not
-- need a 'Monoid' instance for the "IntAdd" state!
applyOpCount :: Op -> Double -> State IntAdd Double
applyOpCount op d = do
  s <- S.get
  S.put (addCosts s (O.opCost op))
  return $ O.applyOp op d

addCosts :: IntAdd -> IntAdd -> IntAdd
addCosts (IntAdd x) (IntAdd y) = IntAdd (x + y)

applyAndCountOperations :: [Op] -> Double -> (Double, IntAdd)
applyAndCountOperations ops d = S.runState (foldl go (S.state (d,)) ops) (IntAdd 0)
  where
    go = (. applyOpCount) . (>>=)

applyOpLog :: Op -> Double -> State [String] Double
applyOpLog op d = do
  s <- S.get
  S.put (s ++ [show op])
  return $ O.applyOp op d

applyAndLogOperations :: [Op] -> Double -> (Double, [String])
applyAndLogOperations ops d = S.runState (foldl go (S.state (d,)) ops) []
  where
    go = (. applyOpLog) . (>>=)

-- Now write these in a simpler way, where the 'State' is the
-- Double itself that you are tracking with the operations.
-- This spares you from needing to track it as a separate input!
applyOpSimple :: Op -> State Double ()
applyOpSimple op = S.modify (fst . applyAndCountOperations [op])

applySimpleOperations :: [Op] -> Double -> Double
applySimpleOperations = (fst .) . applyAndCountOperations
