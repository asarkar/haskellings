module Typeclass.Typeclasses3Test where

import qualified Data.List as L
import Test.Tasty
import Test.Tasty.HUnit
import Typeclass.Typeclasses3 (Adult1 (..), Adult2 (..), InterestRate (..))
import qualified Typeclass.Typeclasses3 as T3

test_Typeclasses3 :: [TestTree]
test_Typeclasses3 =
  [ testCase "Ordering Adults 1" $
      L.sort [Adult1 "Zach" "Whittaker" 31, Adult1 "John" "Smith" 45, Adult1 "Thomas" "Allen" 46]
        @?= [Adult1 "John" "Smith" 45, Adult1 "Thomas" "Allen" 46, Adult1 "Zach" "Whittaker" 31],
    testCase "Ordering Adults 2" $
      L.sort [Adult2 "Zach" "Whittaker" 31, Adult2 "John" "Smith" 45, Adult2 "Thomas" "Allen" 46]
        @?= [Adult2 "Thomas" "Allen" 46, Adult2 "John" "Smith" 45, Adult2 "Zach" "Whittaker" 31],
    testCase "Read Interest Rate" $ map read ["InterestRate 0.5", "InterestRate 0.3", "InterestRate 0.788"] @?= [InterestRate 0.5, InterestRate 0.3, InterestRate 0.788],
    testCase "Higher Interest Rate 1" $
      T3.returnHigherInterestRate ("John", 0.3) ("Tom", 0.03)
        @?= "'\"John\"' has a higher interest rate!",
    testCase "Higher Interest Rate 1" $
      T3.returnHigherInterestRate (Adult1 "John" "Smith" 15, InterestRate 0.07) (Adult1 "Tom" "Allen" 18, InterestRate 0.1)
        @?= "'Adult1 \"Tom\" \"Allen\" 18' has a higher interest rate!"
  ]
