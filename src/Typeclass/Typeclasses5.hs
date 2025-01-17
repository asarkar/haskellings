module Typeclass.Typeclasses5 where

import Control.Arrow ((&&&))
import qualified Control.Monad as M
import qualified Text.Printf as P

{-

- Typeclasses can have dependencies on one another. When defining a typeclass,
  you can give a dependency in much the same way a function can be constrained.
  The simplest example of this is that 'Ord' depends on 'Eq'. We can only order
  elements if they might be equal to one another:

class (Eq a) => Ord a where
  ...

- We just give the dependency, then the sign '=>', and then our class definition
  as usual. This allows you to use functions from the "parent" class in functions
  constrained by your new, dependent class.

printEquals :: (Ord a) => a -> a -> String
printEquals x y = if x == y -- < Can use (==) because of implicit Eq constraint
  then ...

-}

-- TODO:

-- Define a class called Debuggable
-- This class should depend on 'Eq', 'Show', and 'Read'
-- It should have two functions:

-- logObject should take the class variable and a string for the file
-- it was generated from and produce a message

-- compareFromEntry should take a string and the class variable and assess
-- whether or not they are the same, returning a String message and a boolean.

class (Eq a, Show a, Read a) => Debuggable a where
  logObject :: a -> String -> String
  compareFromEntry :: String -> a -> (Bool, String)

-- Create instances of this class for the 'Person' and 'Point3' types.
-- Use the sample output to guide you
data Person = Person String String Int
  deriving stock (Show, Read, Eq)

instance Debuggable Person where
  logObject = P.printf "Produced '%s' from file %s" . show
  compareFromEntry s p =
    if p == read s
      then
        (True, "Found entered Person object equivalent.")
      else
        (False, "Found entered Person object does not match.")

-- logObject (Person "John" "Smith" 32) "Test.hs" -> "Produced 'Person \"John\" \"Smith\" 32' from file Test.hs"
-- compareFromEntry "Person \"John\" \"Smith\" 32" (Person "John" "Smith" 32)
--   -> (True, "Found entered Person object equivalent.")
-- compareFromEntry "Adult \"Jane\" \"Smith\" 32" (Adult "John" "Smith" 32)
--   -> (False, "Found entered Person object does not match.")

data Point3 = Point3 Int Int Int
  deriving stock (Show, Read, Eq)

instance Debuggable Point3 where
  logObject = P.printf "Calculated '%s' from input file %s" . show
  compareFromEntry s p =
    if p == read s
      then
        (True, "New Point calculation matches.")
      else
        (False, "New Point calculation does not match previous.")

-- logObject (Point3 3 4 5) "Test.hs" -> "Calculated 'Point 3 4 5' from input file Test.hs"
-- compareFromEntry (Point3 3 4 5) (Point3 3 4 5)
--   -> (True, "New Point calculation matches.")
-- compareFromEntry (Point3 3 4 5) (Point3 3 4 5)
--   -> (False, "New Point calculation does not match previous.")

-- Fill in this function as a generalization of compareFromEntry
-- Should return a Bool for whether or not the 'read' input matches the given
-- value. The output String should simply 'show' both values on either side of "vs."
--
-- compareAndPrint "Point3 3 4 5" (Point3 6 8 10)
--   -> (False, "'Point3 3 4 5' vs. 'Point3 6 8 10'")
compareAndPrint :: (Debuggable a) => String -> a -> (Bool, String)
--- f &&& g :: String -> (a -> Bool, a -> String)
--- liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
compareAndPrint = M.liftM2 (&&&) f g
  where
    g = (. show) . P.printf "'%s' vs. '%s'"
    f = (fst .) . compareFromEntry
