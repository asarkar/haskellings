module Syntax.Syntax6 where

{-

- A 'where' clause is not the only way to define intermediate values.
  You can also use the keywords 'let' and 'in', like so:

sumEarlyDigits :: Bool -> [Int] -> Int
sumEarlyDigits bool ls =
  let tail1 = tail ls
      second = head tail1
      solution1 = head ls + second
      solution2 = second + head (tail tail1)
  in  if bool then solution1 else solution2

- Follow 'let' with all the expressions you want to define, and once you're
  done you use 'in' to complete the function in terms of your expressions.

sumProducts :: Int -> Int -> Int -> Int
sumProducts x y z =
  let  prod1 = x * y
       prod2 = y * z
       prod3 = x * z
  in   prod1 + prod2 + prod3

- The same rules about ordering apply in a 'let' statement! This circular
  dependency is still bad!

badSum :: Int -> Int -> Int -> Int
badSum x y z = prod1 + prod2 + prod3 + prod4
 let   prod2 = prod1 + (y * z) -- < Can use prod1 even though it's defined "after".
       prod1 = x * y
       prod3 = prod4 + x
       prod4 = y * prod3 -- < This is a problem! prod3 and prod4 depend on each other!
  in   prod1 + prod2 + prod3 + prod4
-}

-- TODO: Re-do these functions from the last part, only use 'let' instead of 'where'!

-- Take the sum of each pairwise product of inputs.
sumPairProducts :: (Int, Int, Int, Int, Int, Int) -> Int
sumPairProducts (a, b, c, d, e, f) =
  let i = e + f
      j = d + i
      k = c + j
      l = b + k
   in a * l + b * k + c * j + d * i + e * f

-- Take the sum of corresponding elements of the tuples, but only include each
-- pair when the corresponding bool is true.
-- e.g. sumTuples (True, False, False) (1, 2, 3) (4, 5, 6) = 5
--      sumTuples (True, False, True)  (1, 2, 3) (4, 5, 6) = 14
sumTuples :: (Bool, Bool, Bool) -> (Int, Int, Int) -> (Int, Int, Int) -> Int
sumTuples (b1, b2, b3) (x1, x2, x3) (y1, y2, y3) =
  let x = if b1 then x1 + y1 else 0
      y = if b2 then x2 + y2 else 0
      z = if b3 then x3 + y3 else 0
   in x + y + z
