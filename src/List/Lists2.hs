module List.Lists2 where

{-

- There are a couple other useful ways to create lists.
  A *range* allows us to quickly define a list with many
  consecutive (or nearly consecutive elements).
  In the most basic usage, a range has two periods (..)
  and then the start and end of the range.

a = [1..6] -- < [1, 2, 3, 4, 5, 6]
b = [1..100]

- By specifying a second number at the beginning, you can
  create a larger interval (or even a negative interval)
  between items in your range!

a = [0,2..10] -- < [0, 2, 4, 6, 8, 10]
b = [21,18..0] -- < [21, 18, 15, 12, 9, 6, 3, 0]

- You can even create *infinite* lists by omitting the end of the range!
  Haskell is *lazily* evaluated, so it will only create the portion of the
  list that is used in your program.

a = [1..] -- < [1, 2, 3, 4, ...]
b = [1,3..] -- < [1, 3, 5, ...]

- The 'take' function allows you to take a certain number of elements from
  any list, and is especially useful for infinite lists.

take :: Int -> [a] -> [a]

take 5 [1..] -- < [1, 2, 3, 4, 5]
take 5 [20, 19..] -- < [20, 19, 18, 17, 16]

- The 'repeat' and 'cycle' functions can also be used to create
  infinite lists.

repeat 1 -- < [1, 1, 1, ...]
cycle [1, 2, 3, 4] -- < [1, 2, 3, 4, 1, 2, 3, 4, 1 ...]

-}

-- TODO: Fill in these functions!

-- Given a particular number, give a list of the numbers starting
-- there and counting down by 5 until you get to 0.
countdownBy5 :: Word -> [Word]
countdownBy5 n = [n, n - 5 .. 0]

-- Given a particular Int, create a list with 100 copies of it.
give100 :: Int -> [Int]
{- HLINT ignore "Use replicate" -}
give100 = take 100 . repeat
