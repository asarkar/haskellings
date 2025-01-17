module List.Lists3 where

{-

- *List comprehensions* allow us to a new way to create lists.
  A list comprehension is an expression with brackets that
  has up to 3 components:

1. The data source
2. An optional filter
3. The resulting data

- Here's one example, which uppercases all the words which are
  5 characters or shorter:

words = ["Hello", "Bye", "Farewell", "Ciao"]
shortWords = [map toUpper word | word <- words, length word <= 5]

shortWords = ["HELLO", "BYE", "CIAO"]

1. The data source is that we are considering each individual element in the
   'words' list:

[...| word <- words, ...]

2. The filter is that we only want to consider words that are shorter:

[... | ... , length word <= 5]

3. The result is that we uppercase the word:

[map toUpper word | ... ]

- So putting it all together, we get:

[map toUpper word | word <- words, length word <= 5]

- We can also take elements from *two different lists*. This will compute every
  pairwise sum and difference from two different lists we provide. Notice that
  we apply no filter this time.

[ x + y | x <- [1..10], y <- [11..20]]

-}

-- TODO: Implement these functions using list comprehensions!

-- Implement addMod3Is2, except now use a list comprehension.
addMod3Is2 :: [Int] -> [Int]
addMod3Is2 xs = [add3 x | x <- xs, mod3Is2 x]
  where
    mod3Is2 x = x `mod` 3 == 2
    add3 = (+ 3)

-- Take every pairwise product of the numbers, as long as their
-- sum is less than 30.
smallPairwiseProducts :: [Int] -> [Int] -> [Int]
smallPairwiseProducts xs ys = [x * y | x <- xs, y <- ys, x + y < 30]
