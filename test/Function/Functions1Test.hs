module Function.Functions1Test where

import qualified Function.Functions1 as F1
import Test.Tasty
import Test.Tasty.HUnit

test_Functions1 :: [TestTree]
test_Functions1 =
  [ testCase "Subtract 1" $ F1.subtract7 13 @?= 6,
    testCase "Subtract 2" $ F1.subtract7 27 @?= 20,
    testCase "Twelve" $ F1.twelve @?= 12,
    testCase "MultiplyProductBy5 1" $ F1.multiplyProductBy5 5 5 @?= 125,
    testCase "MultiplyProductBy5 2" $ F1.multiplyProductBy5 1 10 @?= 50,
    testCase "MultiplyProductBy5 3" $ F1.multiplyProductBy5 2 22 @?= 220,
    testCase "Sixty" $ F1.sixty @?= 60
  ]
