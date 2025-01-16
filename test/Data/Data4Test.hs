module Data.Data4Test where

import qualified Data.Data4 as D4
import Test.Tasty
import Test.Tasty.HUnit

test_Data4 :: [TestTree]
test_Data4 =
  [ testCase "secondAndThird 1" $ D4.secondAndThird [] @?= Nothing,
    testCase "secondAndThird 2" $ D4.secondAndThird [1] @?= Nothing,
    testCase "secondAndThird 3" $ D4.secondAndThird [1, 2] @?= Nothing,
    testCase "secondAndThird 4" $ D4.secondAndThird [1, 2, 3] @?= Just (2, 3),
    testCase "secondAndThird 5" $ D4.secondAndThird [1, 2, 3, 4] @?= Just (2, 3),
    testCase "secondAndThird 6" $ D4.secondAndThird [5, 3, 1, 4, 5, 6] @?= Just (3, 1),
    testCase "secondAndThird' 1" $ D4.secondAndThird' [] @?= Left "Only 0 element(s) in the list",
    testCase "secondAndThird' 2" $ D4.secondAndThird' [1] @?= Left "Only 1 element(s) in the list",
    testCase "secondAndThird' 3" $ D4.secondAndThird' [1, 2] @?= Left "Only 2 element(s) in the list",
    testCase "secondAndThird' 4" $ D4.secondAndThird' [1, 2, 3] @?= Right (2, 3),
    testCase "secondAndThird' 5" $ D4.secondAndThird' [1, 2, 3, 4] @?= Right (2, 3),
    testCase "secondAndThird' 6" $ D4.secondAndThird' [5, 3, 1, 4, 5, 6] @?= Right (3, 1)
  ]
