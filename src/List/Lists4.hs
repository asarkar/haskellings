module List.Lists4 where

{-

- The 'zip' function is very useful for combining lists in an element-by-element way.
  It takes two lists and produces a list of tuples.

zip :: [a] -> [b] -> [(a, b)]

zip [1, 2, 3] ["Hi", "Bye", "Good"] = [(1, "Hi"), (2, "Bye"), (3, "Good")]

- If either list is shorter, the remaining elements of the other list are omitted.

zip [1, 2] ["Hi", "Bye", "Good"] = [(1, "Hi"), (2, "Bye")]
zip [1, 2, 3] ["Hi", "Bye"] = [(1, "Hi"), (2, "Bye")]

- It is a very useful utility, especially combines with items from the last couple
  exercises, like ranges, infinite lists and list comprehensions!

-}

-- Given two lists, match them up element by element and
-- return tuples containing the sum, product, and difference of each pair.
-- sumProductDifference [4, 3] [1, 2] = [(5, 4, 3), (5, 6, 1)]
sumProductDifference :: [Int] -> [Int] -> [(Int, Int, Int)]
sumProductDifference = zipWith (\x y -> (x + y, x * y, x - y))

-- Given a list of strings, make a new list where each String has been
-- appended with its index within the list.
-- addIndices ["Hello", "Bye"] = ["Hello0", "Bye1"]
addIndices :: [String] -> [String]
addIndices = zipWith (flip (++) . show) [0 ..]
