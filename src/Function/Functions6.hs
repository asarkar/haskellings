module Function.Functions6 where

import qualified Data.Char as C

{-

- A function can potentially take any expression as an input.
  Remember though that functions are also expressions!

- This means we can write a function that takes a different function *as an input*.
  We call this a *higher order function*.

- The most simple of these is "map", which transforms every element of a list.

map :: (a -> b) -> [a] -> [b]

- The first input is a function between two types (they can be the same type).
  The second input is the list of items

- **Lambda** syntax allows us to define a function as an expression in the
  middle of our code! This is like writing a normal function, except prefixing
  with a backslash and then listing the arguments, followed by an arrow!
  This technique allows you to use custom functions with other higher order
  functions, like `map`.

altAdder :: Int -> Int -> Int
altAdder = \a b -> a + b

-}

-- So for example, we make a function that transforms integers by doubling them.
double :: Int -> Int
double = (*) 2

-- Then we make a function that makes a new list which doubles every
-- element of its input.
doubleList :: [Int] -> [Int]
doubleList = map double

-- We could also just do: map ((*) 2) xs
-- With **lambda** syntax, we would have: map (\x -> 2 * x) xs

-- TODO: Fill in these functions using map!

-- Flip the boolean value of each input
flipBools :: [Bool] -> [Bool]
flipBools = map not

-- Uppercase all the letters in this word!
capitalizeWord :: String -> String
capitalizeWord = map C.toUpper

-- TODO: Create your own higher order function, doubleAndApply.
-- The input function should take a single integer
-- and produce a tuple of three ints (Int, Int, Int)
-- The doubleAndApply function should then take an extra Int input,
-- double it, and then apply the function.
doubleAndApply :: (Int -> (Int, Int, Int)) -> Int -> (Int, Int, Int)
doubleAndApply = (. (2 *))
