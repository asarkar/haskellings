module Recursion.Recursion1Test where

import qualified Recursion.Recursion1 as R1
import Test.Tasty
import Test.Tasty.HUnit

test_Recursion1 :: [TestTree]
test_Recursion1 =
  [ testCase "Factorial 1" $ R1.factorial 1 @?= 1,
    testCase "Factorial 2" $ R1.factorial 2 @?= 2,
    testCase "Factorial 3" $ R1.factorial 3 @?= 6,
    testCase "Factorial 4" $ R1.factorial 4 @?= 24,
    testCase "Factorial 5" $ R1.factorial 5 @?= 120,
    testCase "Special Distance 0" $ R1.specialDistance 0 @?= 1,
    testCase "Special Distance 1" $ R1.specialDistance 1 @?= 0,
    testCase "Special Distance 2" $ R1.specialDistance 2 @?= 1,
    testCase "Special Distance 3" $ R1.specialDistance 3 @?= 7,
    testCase "Special Distance 4" $ R1.specialDistance 19 @?= 20,
    testCase "Special Distance 5" $ R1.specialDistance 128 @?= 7
  ]
