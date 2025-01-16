module Function.Functions4Test where

import Function.Functions4 ((%=%))
import qualified Function.Functions4 as F4
import Test.Tasty
import Test.Tasty.HUnit

test_Functions4 :: [TestTree]
test_Functions4 =
  [ testCase "multiplyBy6 1" $ F4.multiplyBy6 5 @?= 30,
    testCase "multiplyBy6 2" $ F4.multiplyBy6 (-3) @?= (-18),
    testCase "%=% 1" $ (5 %=% 6) @?= 29,
    testCase "%=% 2" $ (30 %=% 14) @?= 436
  ]
