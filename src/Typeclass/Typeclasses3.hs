module Typeclass.Typeclasses3 where

import qualified Text.Printf as P

{-

- The 'Read' and 'Ord' typeclass are two other useful typeclasses that can
  be easily derived most of the time.

- 'Read' is the "inverse" of 'Show'. It allows us to create a type from a String

read :: String -> a

simpleInts :: [Int]
simpleInts = map read ["1", "2", "3"] -- > [1, 2, 3]

- 'Ord' allows us to provide an "ordering" on objects. That is, we can use
  operators like <, >, >=, and <=. The 'minimal complete definition' requires
  implementing '(<=)' or a 'compare' function. The latter takes two objects
  and produces an "Ordering".

-- Greater Than, Less Than, Equal
data Ordering = GT | LT | EQ

compare :: a -> a -> Ordering

compare 1 2 -> LT
compare "Hello" "Hello" -> EQ
compare "Hello" "Goodbye" -> GT

- So far, all of our constrained functions have had a single input type.
  Note however, that we can have multiple variable types with different
  constraints!

readAndCompare :: (Read a, Eq b) => (a -> b) -> String -> b -> Bool
readAndCompare f input expected = f (read input) == expected

-}

-- TODO:

-- Here we have two different 'Adult' types. Define a different 'Ord'
-- instance for each one. The first should order people by using
-- first name *and then* last name. The second should reverse this,
-- ordering by last name *and then* first name.
-- Try deriving the instance to see which behavior is the default!
data Adult1 = Adult1 String String Int
  deriving stock (Show, Eq)

instance Ord Adult1 where
  compare (Adult1 fName1 lName1 _) (Adult1 fName2 lName2 _) = compare (fName1, lName1) (fName2, lName2)

data Adult2 = Adult2 String String Int
  deriving stock (Show, Eq)

instance Ord Adult2 where
  compare (Adult2 fName1 lName1 _) (Adult2 fName2 lName2 _) = compare (lName1, fName1) (lName2, fName2)

-- Derive both 'Ord' and 'Read' for this type.
newtype InterestRate = InterestRate Double
  deriving stock (Eq, Show, Ord, Read)

-- This function should take two tuples, of variable types (a, b)
-- The 'a' type represents the person, the 'b' type represents
-- their interest rate. Whoever has the higher rate, return a string with
-- "'{p}' has a higher interest rate!", replacing {p} with 'show'-ing the person.
-- Fill in the type signature!
returnHigherInterestRate :: (Show a, Ord i) => (a, i) -> (a, i) -> String
returnHigherInterestRate (p1, i1) (p2, i2) =
  P.printf "'%s' has a higher interest rate!" (show p)
  where
    p = if i1 > i2 then p1 else p2
