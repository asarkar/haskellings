{-# LANGUAGE FlexibleContexts #-}

module Monad.Transformers1 where

import qualified Control.Monad as M
import Control.Monad.State (StateT)
import qualified Control.Monad.State as S
import Monad.Ops (IntAdd (..), Op (..))
import qualified Monad.Ops as O

{-

- Monad *Transformers* allow you to combine the effects of multiple monads.
  These transformers usually consist in taking a normal monad name and
  adding the letter 'T' to the end. Each transformer then has an extra
  parameter for an "underlying" monad. For example, there's 'StateT':

newtype StateT s m a = ...

instance (Monad m) => Monad (StateT s m) where
  ...

- All the same stateful functions we had before still work:

get :: StateT s m s
put :: s -> StateT s m ()
...

- Transformers have 'run' functions like normal monads. However, instead
  of returning a pure result, they return an action in the underlying monad:

runStateT :: StateT s m a -> s -> m (a, s)

- As an example to illustrate this, let's consider using 'IO' as our underlying monad:

doubleReturnTriple :: StateT Double IO Double
doubleReturnTriple = do
  x <- get
  put (2 * x)
  return (3 * x)

stateRunner :: IO (Double, Double)
stateRunner = do
  input <- read <$> getLine
  (result, finalState) <- runStateT doubleReturnTriple 3.0
  print result
  print finalState
  return (result, finalState) -- (9.0, 6.0)

- Using the 'lift' function, you can run a function in the "underlying" monad.
  It works in all transformer cases, but here's what the type signature looks
  like for our case:

lift :: IO a -> StateT s IO a

doubleReturnTriple :: StateT Double IO Double
doubleReturnTriple = do
  x <- get
  lift $ putStr "Starting value: "
  lift $ print x
  lift $ putStrLn "Doubling state!"
  put (2 * x)
  return (3 * x)

-}

-- TODO:

-- Replicate the logic of applyAndCountOperations from the Monads4 exercise.
-- Store the "cost" of the operation in the 'State' value, and update the
-- final 'Double' value.
-- However, you should also print each operation as it is processed.
applyOperations :: [Op] -> Double -> StateT Int IO Double
applyOperations ops d = foldl go (S.state (d,)) ops
  where
    go s op = do
      a <- s
      S.lift $ print a
      S.lift $ print op
      let (IntAdd cost) = O.opCost op
      S.modify (+ cost)
      return $ O.applyOp op a

-- Prompt the user for a number and three operations:
-- 'Please enter a number.'
-- 'Please enter three operations.'
-- Pass these inputs to 'applyOperations'.
-- Print the final tuple result from runState (e.g. 'Result: (1.0, 5)' )
main :: IO ()
main = do
  putStrLn "Please enter a number."
  d <- read <$> getLine
  putStrLn "Please enter three operations."
  ops <- M.replicateM 3 (read <$> getLine)
  S.runStateT (applyOperations ops d) 0 >>= print

{-

Sample Input:

2.0
+ 4.0
\* (-6.0)
√

Sample Output:

Please enter a number.
Please enter three operations.
Adding 4.0
Multiplying by 6.0
Taking Square Root
Result: (6.0, 26)

-}
