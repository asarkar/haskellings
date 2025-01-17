module Monad.Functors where

import Prelude hiding (max, min)

{-

- Understanding *Monads* is a critical piece of learning Haskell. But before we
  can understand monads, we must first understand *Functors*.

- The *Functor* typeclass encapsulates the behavior of a type that
  contains another type and permits transformations over that type.
  It has one function: 'fmap'.

class Functor f where
  fmap :: (a -> b) -> f a -> f b

- This function transforms all of the underlying elements, but leaves
  the structure of the container intact.

- This should sound familiar, because this is exactly what the
  'map' function does for lists! Lists in fact, are a functor, and
  they implement the 'fmap' function by using `map`!

instance Functor [] where
  fmap = map

fmap (+2) [1, 2, 4] -> [3, 4, 6]

- The 'Maybe' and 'Either' types are also functors! They allow us to
  apply a function when we don't know if the underlying value was
  successful or not. If the original value was 'Nothing' or 'Left',
  nothing happens.

fmap (+2) (Just 4) -> Just 6
fmap (+2) Nothing -> Nothing
fmap (+2) (Right 4) -> Right 6
fmap (+2) (Left "Failed") -> Left "Failed"

- Instead of writing out 'fmap', you can also use the (<$>) operator:

(+2) <$> [1, 2, 4] -> [3, 4, 6]
(+2) <$> (Just 4) -> Just 6
(+2) <$> Nothing -> Nothing
(+2) <$> (Right 4) -> Right 6
(+2) <$> (Left "Failed") -> Left "Failed"

-}

safeSquareRoot :: Double -> Maybe Double
safeSquareRoot x = if x < 0 then Nothing else Just (sqrt x)

-- TODO:

-- Rewrite this function so that it uses 'fmap' instead of a case statement!
-- Try to get the definition on one, short line!
multiplySqrtDouble :: Double -> Double -> Maybe Double
multiplySqrtDouble x y = (* 2) <$> safeSquareRoot (x * y)

-- Write a functor instance for this data type!

data Metrics m = Metrics
  { latestMeasurements :: [m],
    average :: m,
    max :: m,
    min :: m,
    mode :: Maybe m
  }
  deriving stock (Show, Eq)

instance Functor Metrics where
  fmap f x =
    Metrics
      (f <$> latestMeasurements x)
      (f (average x))
      (f (max x))
      (f (min x))
      (f <$> mode x)

-- Now write a simple function with fmap to double the value of all the metrics!
doubleMetrics :: Metrics Double -> Metrics Double
doubleMetrics = (<$>) (* 2)
