{-# OPTIONS_GHC -fno-warn-orphans #-}

module Monad.Monads4 where

import Control.Monad.Writer (Writer)
import qualified Control.Monad.Writer as W
import Monad.Ops (IntAdd (..), Op (..))
import qualified Monad.Ops as O

{-

- The *Writer* monad is the next step beyond the Reader monad. It gives us access
  to a "Write-Only" state that we can access when our computation is complete.
  The key is that the state we pass must be a 'Monoid'. So we can only change it
  by "appending" a new value.

instance (Monoid w) => Monad (Writer w) where
  ...

- The primary function you'll use within the Writer monad is 'tell'. Tell takes
  a value of the Monoid type and appends it to the existing state. So if you're
  tracking logs with your Writer, this would be where you append a log message.

tell :: w -> Writer w ()

- Just as Reader has the 'runReader' function, there is also the 'runWriter'
  function, which allows you to run writer computations.

runWriter :: Writer w a -> (a, w)

- The 'runReader' function requires an initial state. But the state is unchanged,
  so the only output is the computation result.

- We can see 'runWriter' is the opposite. There is no initial state for 'w'. It is
  assumed to be the 'mempty' of the Monoid instance. But the function produces both
  the computation values AND the accumulated monoid value.

-}

instance Semigroup IntAdd where
  (IntAdd x) <> (IntAdd y) = IntAdd (x + y)

instance Monoid IntAdd where
  mempty = IntAdd 0

-- TODO:

-- Writer a function that will return the result of the operation AND add
-- its cost to the monad (using 'opCost' above).
-- (If the input to 'Sqrt' is negative, just return the original value)
applyOpCount :: Op -> Double -> Writer IntAdd Double
applyOpCount op d = W.writer (O.applyOp op d, O.opCost op)

-- Now apply a series of operations to an input value using the 'Writer' monad!
applyAndCountOperations :: [Op] -> Double -> (Double, IntAdd)
applyAndCountOperations ops d = W.runWriter $ foldl go (W.writer (d, IntAdd 0)) ops
  where
    go = (. applyOpCount) . (>>=)

-- Do the same as above, except instead of counting the cost, log the
-- string associated with 'showing' the operation.
applyOpLog :: Op -> Double -> Writer [String] Double
applyOpLog op = W.mapWriter ((,[show op]) . fst) . applyOpCount op

applyAndLogOperations :: [Op] -> Double -> (Double, [String])
applyAndLogOperations ops d = W.runWriter $ foldl go (W.writer (d, [])) ops
  where
    go = (. applyOpLog) . (>>=)
