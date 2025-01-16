module Function.Functions3Test where

import qualified Function.Functions3 as F3
import Test.Tasty
import Test.Tasty.HUnit

test_Functions3 :: [TestTree]
test_Functions3 =
  [ testCase "multiplyAndAdd 1" $ F3.multiplyAndAdd (6, -1) (14, 3) @?= 81,
    testCase "multiplyAndAdd 2" $ F3.multiplyAndAdd (-4, -5) (-1, 3) @?= -11,
    testCase "multiplyBy3And4AndAdd 1" $ F3.multiplyBy3And4AndAdd (14, 3) @?= 54,
    testCase "multiplyBy3And4AndAdd 2" $ F3.multiplyBy3And4AndAdd (-1, 3) @?= 9
  ]
