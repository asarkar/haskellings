module Syntax.Syntax2 where

{-

- Nested If-Statements can be annoying to write. So Haskell has a few different
  ways to deal with having several condition branches.

- The first of these are "guards".

function :: InputType -> OutputType
function x
  | {condition :: Bool} = {possible output :: OutputType}
  | {condition :: Bool} = {possible output :: OutputType}
  | otherwise           = {catch all case  :: OutputType}

- Each case has its own line, starting with a vertical bar.
- Then you'll list the condition (which should be a Bool expression).
- The condition is followed by the "equals" sign, and then the return value for that case.
- Instead of "else" for the catch-all case, use the keyword "otherwise".
- Like the two branches of an If-Statement, all branches must have the same output type!

example :: [Int] -> Int
example list1
  | head list1 == 4 = 0
  | head (tail list1) == 4 = 1
  | otherwise = 2

- Observe that the "equals" sign goes on each individual line, NOT after the
  initial function name and arguments!

- Note: The cases are evaluated in the order they are listed.
-}

-- TODO:

-- Rewrite 'countTrue', except this it takes 3 inputs, so use guards!
countTrue :: Bool -> Bool -> Bool -> Int
countTrue b1 b2 b3
  | b1 && b2 && b3 = 3
  | b1 && b2 || b1 && b3 || b2 && b3 = 2
  | b1 || b2 || b3 = 1
  | otherwise = 0

-- Return a string representation of the (positive) input number,
-- from 0 = "Zero" up through 5 = "Five".
-- If it's larger than 5, return "Too many!"
numberString :: Word -> String
numberString n
  | n == 0 = "Zero"
  | n == 1 = "One"
  | n == 2 = "Two"
  | n == 3 = "Three"
  | n == 4 = "Four"
  | n == 5 = "Five"
  | otherwise = "Too many!"
