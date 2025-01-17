module Recursion.Recursion2 where

{-

- Lists are inherently recursive data structures, as we'll explore soon.
  This means recursion is often a good solution for problems involving lists.
  As an example, let's take the sum of a list of Ints

sumList :: [Int] -> Int
...

- If the list is empty, the sum is clearly 0. This is our *base case*.

sumList :: [Int] -> Int
sumList [] = 0
...

- Now we can use a pattern match to deconstruct our list into its first element
  and then *a smaller list*. We can make the recursive call on that list, and
  add the first element!

sumList :: [Int] -> Int
sumList [] = 0                    -- < BASE CASE
sumList (a : as) = a + sumList as -- < RECURSIVE CASE

- Recursion is a 4-step process:

1. Determine the base case
2. Break larger cases into one piece and then a smaller case (or two)
3. Make the recursive call on the smaller case(s)
4. Combine the original piece and the recursive answer(s)

-}

-- TODO: Solve these recursion problems involving lists!

-- Take only the numbers of the input that are equal to "2 mod 3" (2, 5, 8, etc.)
-- Then add 3 to each of them!
-- addMod3Is2 [2, 3, 4, 8] = [5, 10]
addMod3Is2 :: [Int] -> [Int]
addMod3Is2 [] = []
addMod3Is2 (x : xs)
  | x `mod` 3 == 2 = (x + 3) : ys
  | otherwise = ys
  where
    ys = addMod3Is2 xs

-- Take only the 'even' index elements of the list (the second, fourth, sixth, etc.)
-- evens [1, 5, 7, 0, 3, 2, 2, 3] = [5, 0, 2, 3]
evens :: [Int] -> [Int]
evens [] = []
evens [_] = []
evens (_ : x : xs) = x : evens xs
