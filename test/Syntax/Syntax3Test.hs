module Syntax.Syntax3Test where

import Syntax.Syntax3 as S3
import Test.Tasty
import Test.Tasty.HUnit

test_Syntax3 :: [TestTree]
test_Syntax3 =
  [ testCase "numberString 1" $ S3.numberString 1 @?= "One",
    testCase "numberString 2" $ S3.numberString 4 @?= "Four",
    testCase "numberString 3" $ S3.numberString 6 @?= "Too many!",
    testCase "takeN 0 " $ S3.takeN 0 [1, 2, 3, 4, 5] @?= [],
    testCase "takeN 1 " $ S3.takeN 1 [1, 2, 3, 4, 5] @?= [1],
    testCase "takeN 2 " $ S3.takeN 2 [1, 2, 3, 4, 5] @?= [1, 2],
    testCase "takeN 3 " $ S3.takeN 3 [1, 2, 3, 4, 5] @?= [1, 2, 3],
    testCase "takeN 6 " $ S3.takeN 6 [1, 2, 3, 4, 5] @?= [1, 2, 3, 4, 5]
  ]
