module Syntax.Syntax4 where

{-

- When pattern matching on the lists, we can use the structure of the list
  to specify different branches. This includes specifying individual elements
  of the list, limiting its length, or even using the cons operator (:) to
  state that there are "at least" a certain number of items in the list.

evalList :: [Int] -> Int
evalList [] = ...              -- Only evaluates empty list
evalList [x] = ...             -- Evaluates any list with a single element
evalList (1 : 2 : _) = ...     -- List must start with 1 and 2
evalList (x : y : z : _) = ... -- Any list with at least three elements
evalList xs = ...              -- Matches any list

- We can also run pattern matching in the middle of a function by using a "case" statement.
  Each pattern is followed by an arrow, and then the expression. As always, every expression
  must result in the same type.

- The following is equivalent to above:

evalList :: [Int] -> Int
evalList mylist = case myList of
  [] -> ...
  [x] -> ...
  (1 : 2 : _) -> ...
  (x : y : z : _) -> ...
  xs -> ...

-}

-- TODO: Fill in this function on a boolean and a list.
-- If the boolean is set as True, then we care about the
-- number of elements in the list:
-- 0 -> return 0
-- 1 -> return 1 (up through 3 elements)
-- If there are at least 4 elements, return 4.
--
-- If the boolean is set as False, then we care about the
-- sum of the first elements. But if there are at least 4 elements,
-- you can simply return 10.
evalList :: Bool -> [Int] -> Int
evalList b xs =
  if b
    then n
    else case n of
      4 -> 10
      _ -> sum xs
  where
    n = length $ take 4 xs
