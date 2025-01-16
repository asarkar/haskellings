module Data.Data6Test where

import Data.Data6 (Intercept (..), Slope (..), XCoordinate (..), YCoordinate (..))
import qualified Data.Data6 as D6
import Test.Tasty
import Test.Tasty.HUnit

-- Change these to account for newtypes!
x1 :: XCoordinate
x1 = XCoordinate 4.0

s1 :: Slope
s1 = Slope (-3.0)

i1 :: Intercept
i1 = Intercept 2.0

y1 :: YCoordinate
y1 = D6.calculateY s1 i1 x1

expectedY1 :: YCoordinate
expectedY1 = YCoordinate (-10.0)

x2 :: XCoordinate
x2 = XCoordinate 3.5

s2 :: Slope
s2 = Slope 2.3

i2 :: Intercept
i2 = Intercept 3.7

y2 :: YCoordinate
y2 = YCoordinate 11.75

test_Data6 :: [TestTree]
test_Data6 =
  [ testCase "Slope 1" $ D6.calculateY s1 i1 x1 @?= expectedY1,
    testCase "Slope 2" $ D6.calculateY s2 i2 x2 @?= y2
  ]
