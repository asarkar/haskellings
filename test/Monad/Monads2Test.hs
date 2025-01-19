module Monad.Monads2Test where

import qualified Monad.Monads2 as M2
import Test.Tasty
import Test.Tasty.HUnit

test_Monads2 :: [TestTree]
test_Monads2 =
  [ testCase "sumOfSquareRoots 1" $ M2.sumOfSquareRoots 4.0 9.0 @?= Just 5.0,
    testCase "sumOfSquareRoots 2" $ M2.sumOfSquareRoots (-4.0) 9.0 @?= Nothing,
    testCase "sumOfSquareRoots 3" $ M2.sumOfSquareRoots (-4.0) (-9.0) @?= Nothing,
    testCase "sumOfSquareRoots 4" $ M2.sumOfSquareRoots 4.0 (-9.0) @?= Nothing,
    testCase "sumOfSquareRoots 5" $ M2.sumOfSquareRoots 4.0 0.0 @?= Just 2.0,
    testCase "generateAllResults 1" $ M2.generateAllResults ([] :: [Int -> Int -> Int]) [1, 2] [3, 4] @?= [],
    testCase "generateAllResults 2" $ M2.generateAllResults [(+) :: Int -> Int -> Int, (*)] [] [3, 4] @?= [],
    testCase "generateAllResults 3" $ M2.generateAllResults [(+)] [1, 2] [3, 4] @?= [4, 5, 5, 6],
    testCase "generateAllResults 4" $ M2.generateAllResults [(+), (*)] [1, 2] [3, 4] @?= [4, 5, 5, 6, 3, 4, 6, 8],
    testCase "generateAllResults 5" $ M2.generateAllResults [(+), (*), (-)] [10] [3, 4, 5] @?= [13, 14, 15, 30, 40, 50, 7, 6, 5],
    testCase "sqrtAndMultiply 1" $ M2.sqrtAndMultiply 0.0 @?= Just 0.0,
    testCase "sqrtAndMultiply 2" $ M2.sqrtAndMultiply 4.0 @?= Just 20.0,
    testCase "sqrtAndMultiply 3" $ M2.sqrtAndMultiply 81.0 @?= Just 90.0,
    testCase "sqrtAndMultiply 4" $ M2.sqrtAndMultiply 121.0 @?= Nothing,
    testCase "sqrtAndMultiply 5" $ M2.sqrtAndMultiply 144.0 @?= Nothing,
    testCase "sqrtAndMultiply 6" $ M2.sqrtAndMultiply (-4.0) @?= Nothing,
    testCase "addAndNegate 1" $ M2.addAndNegate [] @?= [],
    testCase "addAndNegate 2" $ M2.addAndNegate [10] @?= [11, -11, 12, -12, 13, -13],
    testCase "addAndNegate 3" $ M2.addAndNegate [1, 2] @?= [2, -2, 3, -3, 4, -4, 3, -3, 4, -4, 5, -5]
  ]
