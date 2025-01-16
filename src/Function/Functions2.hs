module Function.Functions2 where

import Control.Arrow ((***))
import qualified Data.Char as C

{-

- There are many functions already out there that we can call on our different types.
  Here are a few examples:

1. head :: [a] -> a
   returns the first element of any non-empty list ("a" is a generic type)
   head [1, 2, 3] = 1

2. tail :: [a] -> [a]
   returns everything _but_ the first element of a non-empty list
   tail [1, 2, 3] = [2, 3]

3. fst :: (a, b) -> a
   returns the first element of a tuple
   fst (True, []) = True

4. snd :: (a, b) -> b
   returns the second element of a tuple
   snd (True, []) = []

5. toUpper :: Char -> Char
   Uppercases an ASCII character
   toUpper 'a' = 'A'

6. toLower :: Char -> Char
   Lowercases an ASCII character
   toUpper 'Z' = 'z'

-}

-- TODO: Fill in these functions!

-- Add the heads of the two inputs together. Return this as the first element
-- of the tuple. Then you should also return the tail of each list.
addHeads :: ([Int], [Int]) -> (Int, [Int], [Int])
addHeads (x : xs, y : ys) = (x + y, xs, ys)
addHeads _ = error "empty list"

-- Produce a new tuple containing the three *second* elements of each of
-- the input tuples.
takeSeconds :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int, Int)
takeSeconds x y z = (snd x, snd y, snd z)

-- Capitalize each of the two characters.
capitalize :: (Char, Char) -> (Char, Char)
capitalize = C.toUpper *** C.toUpper
