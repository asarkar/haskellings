{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Typeclass.Typeclasses2 where

import qualified Text.Printf as P

{-

- Many times, we don't want to (or can't) use the default instances we get
  from using 'deriving'. In these cases we'll have to define our own instances.
  In this case we use the 'instance' keyword like so:

data Person = Person String Int

instance Show Person where
  ...

- Then within the instance, we have to provide a function implementation for each
  function within the class.

instance Show Person where
  show (Person name salary) = name ++ " makes $" ++ show salary ++ " per year."

- Some classes have a "minimal complete definition". This means we don't have to
  define every function, because it can derive some functions from the others.

- For example, with Eq, we only have to implement (==) and not (/=), because it
  can use the definition of (==) to fill in (/=)

instance Eq Person where
  (==) (Person n1 _) (Person n2 _) = n1 == n2 -- < Ignore salaries

-}

-- TODO: Write manual definitions of 'Show' and 'Eq' for these types, with
--       the denoted changes from the default definitions.

-- For the 'Show' instance, use lowercase for the first letter.
data Occupation = Lawyer | Programmer | Engineer | Doctor | Manager | Teacher

instance Show Occupation where
  show Lawyer = "lawyer"
  show Programmer = "programmer"
  show Engineer = "engineer"
  show Doctor = "doctor"
  show Manager = "manager"
  show Teacher = "teacher"

instance (Show Occupation) => Eq Occupation where
  x == y = show x == show y

-- Consider two 'Persons' "equal" as long as the constructor and name matches.
-- Use the first two string fields for Adults and the first field for Children
-- For 'Show', you should only use the full name and age:
--   "John Smith is 32 years old"
--   "Chris is 12 years old"
data Person
  = Adult String String Int Occupation
  | Child String Int Int

instance Show Person where
  show (Adult fName lName age _) = P.printf "%s %s is %d years old" fName lName age
  show (Child name age _) = P.printf "%s is %d years old" name age

instance Eq Person where
  (Adult fName1 lName1 _ _) == (Adult fName2 lName2 _ _) = fName1 == fName2 && lName1 == lName2
  (Child name1 _ _) == (Child name2 _ _) = name1 == name2
  _ == _ = False

-- The 'Show' instance should omit 'InterestRate'. Just show the underlying Double.
newtype InterestRate = InterestRate Double

instance Show InterestRate where
  show (InterestRate rate) = show rate

instance Eq InterestRate where
  (InterestRate x) == (InterestRate y) = x == y
