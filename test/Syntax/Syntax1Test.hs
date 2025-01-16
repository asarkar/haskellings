module Syntax.Syntax1Test where

import Syntax.Syntax1 as S1
import Test.Tasty
import Test.Tasty.HUnit

test_Syntax1 :: [TestTree]
test_Syntax1 =
  [ testCase "countTrue 1" $ S1.countTrue True False @?= 1,
    testCase "countTrue 2" $ S1.countTrue False False @?= 0,
    testCase "countTrue 3" $ S1.countTrue False True @?= 1,
    testCase "countTrue 4" $ S1.countTrue True True @?= 2,
    testCase "evalInput 1" $ S1.evalInput 0 [1, 2] @?= 1.0,
    testCase "evalInput 2" $ S1.evalInput 1 [0, 2] @?= 2.5,
    testCase "evalInput 3" $ S1.evalInput 5 [3, 2] @?= 3.5
  ]
