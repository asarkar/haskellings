module Monad.Monoids where

import qualified Data.Monoid as M

{-

- Monoids and Semigroups are two more types of functional structures.
  They aren't as connected to Monads as Functors and Applicatives, but
  we'll still need them.

- A Semigroup is any type that is "appendable". That is, we can take
  two elements of the type and combine them into a new element within the type.
  This behavior is captured by the append operator (<>):

class Semigroup a where
  (<>) :: a -> a -> a

- The clearest example of a semigroup is, of course, a list. The 'append'
  function is simply the list append operator (++) we've been using:

instance Semigroup [a] where
  (<>) = (++)

- A 'Monoid' is an extension of a Semigroup. What defines a Monoid is that
  there is a characteristic "Identity" element called 'mempty'. If this
  object is appended on either side of another element 'a', then the result
  is still equivalent to 'a'. For a list, this element is clearly '[]', the empty list.

class (Semigroup a) => Monoid a where
  mempty :: a

instance Monoid [a] where
  mempty = []

-}

-- TODO:

-- Write Semigroup and Monoid instances for these two types.
-- In the first, "appending" should work like addition.
-- In the second, "appending" should work like multiplication.

newtype IntAdd = IntAdd Int
  deriving stock (Show, Eq)

instance Semigroup IntAdd where
  (IntAdd x) <> (IntAdd y) = IntAdd (x + y)

instance Monoid IntAdd where
  mempty = IntAdd 0

newtype IntMultiply = IntMultiply Int
  deriving stock (Show, Eq)

instance Semigroup IntMultiply where
  (IntMultiply x) <> (IntMultiply y) = IntMultiply (x * y)

instance Monoid IntMultiply where
  mempty = IntMultiply 1

-- Write a function that takes any two items of an "appendable" type.
-- The result should be the two items appended in an "ABBA" pattern.
-- abba [1, 2] [3] = [1, 2, 3, 3, 1, 2]
-- abba (IntMultiply 4) (IntMultiply 6) = IntMultiply (4 * 6 * 6 * 4) -- 576
-- Write the type signature yourself.
abba :: (Monoid a) => a -> a -> a
abba x y = M.mconcat [x, y, y, x]
