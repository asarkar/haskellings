module Monad.Applicatives where

import qualified Control.Applicative as A

{-

- Once you understand Functors, you can move on to *Applicative Functors*,
  often shortened as *Applicatives*. The name comes from the term
  "function *application*".

- A Functor stores information, and we apply functions over the information
  in the structure. But applicatives have tools that allow us to store
  the function itself in the structure and apply the stored function based
  on the structure's rules.

- Let's start with the class definition. This has two functions, instead
  of Functor, which only had 1:

class Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

- The first function, 'pure', tells us how to wrap an element in the
  structure in the most basic way.
- The second function, the "apply operator" takes a transformation within
  the structure, the structure containing the first type, and performs the
  transformation over the whole structure.

- Notice the difference with 'fmap'. With an applicative, the function itself
  is wrapped.

(<$>) ::   (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b

- Maybe, Either, and List are Applicative Functors, in addition to being Functors!
  The instances for Maybe and Either are intuitive. We only create a "success" if
  both inputs are wrapped as success.

instance Applicative Maybe where
  pure a = Just a
  Just f <*> Just a = Just (f a)
  _ <*> _ = Nothing

instance Applicative (Either e) where
  pure a = Right a
  Right f <*> Right a = Right (f a)
  Right f <*> Left e = Left e
  Left e <*> _ = Left e

- With a List, the applicative is defined as a comprehension! We apply every
  function to every element in the second list!

instance Applicative [] where
  pure a = [a]
  fs <*> xs = [f x | f <- fs, x <- xs]

-}

safeSquareRoot :: Double -> Maybe Double
safeSquareRoot x = if x < 0 then Nothing else Just (sqrt x)

-- TODO: Fill in the following functions using applicatives!

-- Return the sum of the square roots of the two inputs, using
-- 'safeSquareRoot' to check for negatives.
sumOfSquareRoots :: Double -> Double -> Maybe Double
sumOfSquareRoots = (. safeSquareRoot) . A.liftA2 (+) . safeSquareRoot

-- Generate all combinations of sums between the first list and the second list.
-- (avoid using a list comprehension)
generateSums :: [Int] -> [Int] -> [Int]
generateSums = A.liftA2 (+)

-- Given a list of operations and two lists, generate all combinations of
-- those operations with each pair of numbers from the two lists.
-- generateAllResults [(+), (*)] [1, 2] [3, 4] -> [4, 5, 5, 6, 3, 4, 6, 8]
generateAllResults :: [Int -> Int -> Int] -> [Int] -> [Int] -> [Int]
generateAllResults = A.liftA3 id
