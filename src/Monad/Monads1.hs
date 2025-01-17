module Monad.Monads1 where

import Control.Monad ((<=<))
import qualified Control.Monad as M

{-

- With our knowledge of Functors and Applicatives, we can now learn about *Monads*.
  Just as every Applicative must be a Functor, every Monad must already be an Applicative.

- A *Monad* wraps a value in a particular *computational context*.
  We'll see what this means through example. But let's start with the typeclass.
  As with Applicative, the Monad class has two main functions.

class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b

- The 'return' function is essentially the same as Applicative 'pure', and
  almost always they are implemented the same way. It just wraps a
  value in a minimal context (or structure).

- The (>>=) operator is called the 'bind' operator. It allows us to take
  a value in the Monad context and apply a function against the unwrapped
  value. The key is that the function must re-wrap the value in the context.
  You can only *remove* a value from its context completely in special ways, which we'll see.

- 'Maybe' is a Monad. It wraps its values in the context of *possible failure*.
  If a computation has failed, it is 'Nothing', and subsequent computations that
  depend on this value should also be 'Nothing'. We can see this in the implementation
  of (>>=)

instance Monad Maybe where
  return a = Just a
  Nothing >>= _ = Nothing
  Just a >>= f = f a

- Lists are also a Monad. Their context is that the computation can have many
  different results (or no results!). Given we have a function that takes one
  input and produces many results, we can bind this to many different inputs
  using a list comprehension.

instance Monad [] where
  return a = [a]
  xs >>= f = [y | x <- xs, y <- f x]

- If we flip the bind operator around (=<<), we can see the clear pattern between
  Functors, Applicatives, and Monads!

(<$>) ::   (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b
(=<<) :: (a -> f b) -> f a -> f b

-}

safeSquareRoot :: Double -> Maybe Double
safeSquareRoot x = if x < 0 then Nothing else Just (sqrt x)

multiplyIfSmall :: Double -> Double -> Maybe Double
multiplyIfSmall y x = if x < 9.5 then Just (y * x) else Nothing

-- TODO:

-- Using the monad bind operator, apply the safeSquareRoot to the input
-- and then multiply it by 10 using multiplyIfSmall.
-- sqrtAndMultiply (-3.0) = Nothing
-- sqrtAndMultiply 25.0 = Just 50.0
-- sqrtAndMultiply 121.0 = Nothing
sqrtAndMultiply :: Double -> Maybe Double
sqrtAndMultiply = (=<<) (multiplyIfSmall 10) . safeSquareRoot

-- Given a list of inputs, produce a new list that adds 1, 2, and 3 to each input
-- and then for the final result, also includes the negation of every input.
-- addAndNegate [1, 2] -> [2, -2, 3, -3, 4, -4, 3, -3, 4, -4, 5, -5]
addAndNegate :: [Int] -> [Int]
-- (flip id) :: b -> (b -> c) -> c
-- (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
-- Written in the infix notation, (a -> m b) -> a part is on the right.
-- ap :: Monad m => m (a -> b) -> m a -> m b
-- (ap (:)) :: (a -> [a]) -> a -> [a]
-- (return . negate) :: (Monad m, Num b) => b -> m b
-- (ap (:) (return . negate)) :: Num a => a -> [a]
addAndNegate = M.ap (:) (return . negate) <=< (((<$> [(1 +), (2 +), (3 +)]) . flip id) =<<)
