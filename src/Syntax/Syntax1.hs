module Syntax.Syntax1 where

{-

- "If" statements are different in Haskell from most other languages
  in that they *must* have an "else" branch.

- The general syntax looks like:
  if {Boolean} then {true branch} else {false branch}

- An If-Statement is an expression, and so it must have a type. In
  order for it to have a type, both branches must exist and they
  must be expressions of the *same type*.

- You can use 1 line or multiple lines for an "if" statement.

-}

-- Here's an example.
-- Note: "mod" is a useful function giving us the modulus of an integer.
--       Also, we can compare basic types using "==", like in other languages.
doubleIfOdd :: Int -> Int
doubleIfOdd x =
  if mod x 2 == 1
    then 2 * x
    else x

-- There is no "elif" like in Python. To add additional branches, make another
-- "if" statement in the "else" branch.
multiplicationFunc :: Int -> Int
{- HLINT ignore "Use guards" -}
multiplicationFunc x =
  if mod x 3 == 0
    then x * 3
    else
      if mod x 3 == 1
        then x * 6
        else x * 8

-- TODO: Fill in these functions!
-- Of the two inputs, return how many are "True"
countTrue :: Bool -> Bool -> Int
{- HLINT ignore "Use guards" -}
countTrue b1 b2 =
  if b1 && b2
    then 2
    else
      if b1 || b2
        then 1
        else 0

-- What is the type signature of this function?
evalInput :: Int -> [Int] -> Float
{- HLINT ignore "Use guards" -}
evalInput x myList =
  if x == 0
    then 1.0
    else
      if head myList == 0
        then 2.5
        else 3.5

-- The following will not work, since the "else" branch has a different type.
-- TODO: Try uncommenting this when you're finished and check that it
--       doesn't compile.
-- badIf x = if x == 3 then 45 else "Hello"
