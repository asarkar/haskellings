module Data.Data2 where

{-

- Haskell data types can also have multiple constructors! When we want
  this, we separate the constructors with a vertical bar character '|'.

- We've already seen an example of this in the "Bool" type.
  'True' and 'False' are its two constructors.

data Bool = True | False

- This makes it easy for us to let our data take different forms with
  different associated data.

data Calculation =
  RawResult Double |
  LoggedResult Double String

- You can branch on the constructor of your input with pattern matching
  and case statements:

doubleCalculation :: Calculation -> Double
doubleCalculation (RawResult x) = 2 * x
doubleCalculation (LoggedResult x _) = 2 * x

-}

-- TODO

-- First define an "Occupation" type with many different constructors
-- for what a person's job could be. You should at least include
-- 'Engineer' and 'Lawyer' as constructors. You don't need any extra data
-- for each constructor, like with Bool!

data Occupation = Lawyer | Engineer

-- Take your two types from the last part and combine them into a single
-- 'Person' type with two constructors. Now use your new "Occupation" type
-- for the Adult's job instead of a string.
data Person
  = Adult String String Int Occupation
  | Child String Int Int

-- For adults, return their first and last name appended, with a space in between.
-- For children, just return their first name.
giveFullName :: Person -> String
giveFullName (Adult fName lName _ _) = fName ++ " " ++ lName
giveFullName (Child name _ _) = name
