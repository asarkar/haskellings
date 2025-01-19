module Monad.Monads1Test where

import qualified Monad.Monads1 as M1
import Test.Tasty
import Test.Tasty.HUnit

test_Monads1 :: [TestTree]
test_Monads1 =
  [ testCase "sqrtAndMultiply 1" $ M1.sqrtAndMultiply 0.0 @?= Just 0.0,
    testCase "sqrtAndMultiply 2" $ M1.sqrtAndMultiply 4.0 @?= Just 20.0,
    testCase "sqrtAndMultiply 3" $ M1.sqrtAndMultiply 81.0 @?= Just 90.0,
    testCase "sqrtAndMultiply 4" $ M1.sqrtAndMultiply 121.0 @?= Nothing,
    testCase "sqrtAndMultiply 5" $ M1.sqrtAndMultiply 144.0 @?= Nothing,
    testCase "sqrtAndMultiply 6" $ M1.sqrtAndMultiply (-4.0) @?= Nothing,
    testCase "addAndNegate 1" $ M1.addAndNegate [] @?= [],
    testCase "addAndNegate 2" $ M1.addAndNegate [10] @?= [11, -11, 12, -12, 13, -13],
    testCase "addAndNegate 3" $ M1.addAndNegate [1, 2] @?= [2, -2, 3, -3, 4, -4, 3, -3, 4, -4, 5, -5]
  ]
