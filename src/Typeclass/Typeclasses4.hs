module Typeclass.Typeclasses4 where

{-

- There will, of course, be times where we want to define our own typeclasses
  and common behaviors. We can define a typeclass with the 'class' keyword.
  We then continue with the name of our class then a lowercase variable name.
  Then, as with our instance, we use the 'where' keyword:

class Mathable a where
  ...

- Then you simply list the function names and type signatures for the
  different functions you want in that class! These will almost always
  contain the variable type:

-}

class Mathable a where
  getSum :: a -> Int
  getProduct :: a -> Int
  getMin :: a -> Int
  getMax :: a -> Int

-- Here's an instance for this class:
data Point3 = Point3 Int Int Int

instance Mathable Point3 where
  getSum (Point3 a b c) = a + b + c
  getProduct (Point3 a b c) = a * b * c
  getMin (Point3 a b c) = min a (min b c)
  getMax (Point3 a b c) = max a (max b c)

-- TODO:

-- Make a class 'HasName'
-- This should have two functions that take the target type and return a String
-- 'getName' should return the object's full name.
-- 'greet' should return the string "Hello there, " and then append the full name.

class HasName a where
  getName :: a -> String
  greet :: a -> String
  greet = (++) "Hello there, " . getName

-- Make an instance of this class for both 'Adult' and 'Child'
data Adult = Adult
  { firstName :: String,
    lastName :: String,
    adultAge :: Int
  }

instance HasName Adult where
  getName a = firstName a ++ " " ++ lastName a

data Child = Child
  { name :: String,
    childAge :: Int,
    grade :: Int
  }

instance HasName Child where
  getName = name

-- Take two objects of potentially different types and give an ordering based
-- on their full name (i.e. using getName)
-- (Add constraints as necessary)
compareByName :: (HasName a, HasName b) => a -> b -> Ordering
compareByName = (. getName) . compare . getName
