module Syntax.Syntax3 where

{-

- Pattern Matching is a another powerful tool to branch our code behavior based
  on the values or structure of our input elements.

- Instead of assigning a name to our inputs, we can use a value on the left side
  to indicate what the behavior of the function should be when the input has
  that value. We do this for multiple different values, essentially defining
  multiple versions of our function depending on the input.

-}

-- Here's an example where we have different behavior when the input
-- boolean is True vs. False
addOrSubtract :: Bool -> Int -> Int -> Int
addOrSubtract True x y = x + y
addOrSubtract False x y = x - y

-- Here's our we might write 'countTrue' with 2 inputs:
-- We don't need the input values in the last pattern, so we use
-- underscores ('_') as placeholders. If we needed it in the function
-- definition, we could assign names like normal.
countTrue :: Bool -> Bool -> Int
countTrue True True = 2
countTrue False False = 0
countTrue _ _ = 1

-- TODO:

-- Rewrite numberString from last time, but use a pattern match instead of guards!
numberString :: Word -> String
numberString 0 = "Zero"
numberString 1 = "One"
numberString 2 = "Two"
numberString 3 = "Three"
numberString 4 = "Four"
numberString 5 = "Five"
numberString _ = "Too many!"

-- If the input number is 0-3, return the first, corresponding number of elements
-- in the list. e.g. takeN 0 [1,2,3,4] = [], takeN 2 [1,2,3,4] = [1,2]
-- If the input number is larger, return the whole list.
-- You can assume the input has at least 4 elements.
takeN :: Word -> [Int] -> [Int]
takeN 0 = const []
takeN 1 = take 1
takeN 2 = take 2
takeN 3 = take 3
takeN _ = id
