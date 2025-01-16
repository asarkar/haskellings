module Function.Functions6Test where

import qualified Function.Functions6 as F6
import Test.Tasty
import Test.Tasty.HUnit

test_Functions6 :: [TestTree]
test_Functions6 =
  [ testCase "flipBools 1" $ F6.flipBools [True, False, True] @?= [False, True, False],
    testCase "flipBools 2" $ F6.flipBools [False, False, False, False] @?= [True, True, True, True],
    testCase "capitalizeWord 1" $ F6.capitalizeWord "Hello" @?= "HELLO",
    testCase "capitalizeWord 2" $ F6.capitalizeWord "abcdefghzyx" @?= "ABCDEFGHZYX",
    testCase "doubleAndApply 1" $ F6.doubleAndApply (\i -> (i + 1, i + 2, i + 3)) 5 @?= (11, 12, 13),
    testCase "doubleAndApply 2" $ F6.doubleAndApply (\i -> (i * 2, i * 3, i * 4)) 5 @?= (20, 30, 40)
  ]
