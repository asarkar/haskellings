module Function.Functions3 where

{-

- Suppose we have a function that takes two arguments. If we only apply
  *one* of the arguments, we get a new function that takes only one argument.

-}

multiplyBy3AndAdd :: Int -> Int -> Int
multiplyBy3AndAdd x y = y * 3 + x

-- We take the function above and apply a single argument.
-- Thus we still have to take one more "Int"!
multiplyBy3Add5 :: Int -> Int
multiplyBy3Add5 = multiplyBy3AndAdd 5

-- multiplyBy3AndAdd 4 5 = 19
-- multiplyBy3AndAdd 10 -3 = 1
-- multiplyBy3Add5 5 = 20
-- multiplyBy3Add5 -3 = -4

-- TODO: Fill in these functions!

-- Take two tuples.
-- Multiply the first elements of each tuple together.
-- Then multiply the second elements together as well.
-- Add the results.
multiplyAndAdd :: (Int, Int) -> (Int, Int) -> Int
multiplyAndAdd (a, b) (c, d) = (a * c) + (b * d)

-- Take a tuple of two Ints.
-- Multiply the first value by 3 and the second by 4. Then add the results.
multiplyBy3And4AndAdd :: (Int, Int) -> Int
multiplyBy3And4AndAdd = multiplyAndAdd (3, 4)
