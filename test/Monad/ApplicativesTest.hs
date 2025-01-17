module Monad.ApplicativesTest where

import qualified Monad.Applicatives as A
import Test.Tasty
import Test.Tasty.HUnit

test_Applicatives :: [TestTree]
test_Applicatives =
  [ testCase "sumOfSquareRoots 1" $ A.sumOfSquareRoots 4.0 9.0 @?= Just 5.0,
    testCase "sumOfSquareRoots 2" $ A.sumOfSquareRoots (-4.0) 9.0 @?= Nothing,
    testCase "sumOfSquareRoots 3" $ A.sumOfSquareRoots (-4.0) (-9.0) @?= Nothing,
    testCase "sumOfSquareRoots 4" $ A.sumOfSquareRoots 4.0 (-9.0) @?= Nothing,
    testCase "sumOfSquareRoots 5" $ A.sumOfSquareRoots 4.0 0.0 @?= Just 2.0,
    testCase "generateSums 1" $ A.generateSums [] [] @?= [],
    testCase "generateSums 2" $ A.generateSums [] [4, 5, 6] @?= [],
    testCase "generateSums 3" $ A.generateSums [4, 5, 6] [] @?= [],
    testCase "generateSums 4" $ A.generateSums [4, 5, 6] [2] @?= [6, 7, 8],
    testCase "generateSums 5" $ A.generateSums [1, 2, 3] [4, 5, 6] @?= [5, 6, 7, 6, 7, 8, 7, 8, 9],
    testCase "generateSums 6" $ A.generateSums [10, 8, 11, 6] [3, 4, -6] @?= [13, 14, 4, 11, 12, 2, 14, 15, 5, 9, 10, 0],
    testCase "generateAllResults 1" $ A.generateAllResults ([] :: [Int -> Int -> Int]) [1, 2] [3, 4] @?= [],
    testCase "generateAllResults 2" $ A.generateAllResults [(+) :: Int -> Int -> Int, (*)] [] [3, 4] @?= [],
    testCase "generateAllResults 3" $ A.generateAllResults [(+)] [1, 2] [3, 4] @?= [4, 5, 5, 6],
    testCase "generateAllResults 4" $ A.generateAllResults [(+), (*)] [1, 2] [3, 4] @?= [4, 5, 5, 6, 3, 4, 6, 8],
    testCase "generateAllResults 5" $ A.generateAllResults [(+), (*), (-)] [10] [3, 4, 5] @?= [13, 14, 15, 30, 40, 50, 7, 6, 5]
  ]
