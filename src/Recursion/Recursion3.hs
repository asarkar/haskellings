module Recursion.Recursion3 where

import qualified Data.Tuple as T

{-

- *Tail recursion* is a special kind of recursion. Our goal is that the
  recursive call is the *last* thing our recursive case actually has to
  do. That is, there is no additional work to combine a previous piece
  after a recursive answer is retrieved.

- Haskell code is optimized so that tail recursion is more memory efficient,
  which allows us to call our functions on larger inputs.

- We track the work we've done by making a helper function that takes an
  *accumulation argument*. In our 'sumList' example, we'll have this argument
  represent the sum of elements we've already seen. Then we call our helper
  function with a trivial first input:

sumList :: [Int] -> Int
sumList xs = sumListTail xs 0 -- < Initial accumulator is 0
  where
    sumListTail :: [Int] -> Int -> Int
    sumListTail [] accum = accum -- < BASE CASE - Return accumulator
    sumListTail (x : xs) accum = sumListTail xs (x + accum) -- < RECURSIVE CASE
-}

-- TODO:

-- Write a tail-recursive function to reverse the list!
-- (Note: In real code, you can rely on the 'reverse' library function)
reverseList :: [a] -> [a]
reverseList = go []
  where
    go acc [] = acc
    go acc (x : xs) = go (x : acc) xs

-- Given a list, return a tuple of two sub-lists.
-- The first is the "even" elements like you did in the last exercise!
-- The second is the "odd" elements.
-- Do this tail recursively!
-- evenOdds [1, 5, 7, 0, 3, 2, 2, 3] = ([5, 0, 2, 3], [1, 7, 3, 2])
evenOdds :: [a] -> ([a], [a])
evenOdds = go ([], [])
  where
    go acc [] = T.swap acc
    go acc (x : xs) = (zs, x : ys)
      where
        (ys, zs) = go (T.swap acc) xs
