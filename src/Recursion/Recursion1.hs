module Recursion.Recursion1 where

{-

- *Recursion* is a fundamental concept in Haskell.
  A *recursive* function calls itself within its definition.
  This might seem like it will continue infinitely.
  If you aren't careful, it will!

- The key to recursion is having a *base case*. When your input
  is sufficiently simple, the answer is easy and obvious!
  When the input is larger or more complicated, you do a little
  bit of work, and then call the function again on a *smaller, less complicated input*.

- Positive integer division is a simple function we can write recursively.
  When the dividend (first input) is smaller than the divisor (second),
  the answer is 0. This is our *base case*.

quotient :: Word -> Word -> Word
quotient dividend divisor = if dividend < divisor
  then 0   -- < BASE CASE
  else ...

- What if it's larger? We can subtract the divisor from the dividend.
  Then we can get the quotient of that *smaller* case, and add 1!
  This is the *recursive case*, since we call 'quotient' again.

quotient :: Word -> Word -> Word
quotient dividend divisor = if dividend < divisor
  -- BASE CASE
  then 0
  -- RECURSIVE CASE
  else 1 + (quotient (dividend - divisor) divisor)

- Since the recursive case has a smaller input, it will eventually reach the
  base case and our call stack will terminate!

-}

-- TODO: Complete these recursive functions!

-- Calculator the factorial as 1 * 2 * ... * n
-- factorial 1 = 1
-- factorial 2 = 2 -- (1 * 2)
-- factorial 3 = 6 -- (1 * 2 * 3)
-- ...
factorial :: Int -> Int
factorial n
  | n <= 0 = 0
  | n == 1 = 1
  | otherwise = n * factorial (n - 1)

-- Calculate the 'specialDistance' to 1, according to these rules:
-- The distance from 1 to itself is 0
-- The distance from 0 to 1 is 1
-- If the input n is even, add 1 to the distance to n / 2.
-- If the input n is odd, add 1 to the distance to 3n + 1.
--  NOTE: The actual library function for integer division is 'quot'.
specialDistance :: Word -> Word
specialDistance n
  | n == 0 = 1
  | n == 1 = 0
  | even n = 1 + specialDistance (n `quot` 2)
  | otherwise = 1 + specialDistance (3 * n + 1)
