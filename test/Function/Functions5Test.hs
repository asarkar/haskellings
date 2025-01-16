module Function.Functions5Test where

import qualified Function.Functions5 as F5
import Test.Tasty
import Test.Tasty.HUnit

test_Functions5 :: [TestTree]
test_Functions5 =
  [ testCase "trueSink 1" $ F5.trueSink True @?= True,
    testCase "trueSink 2" $ F5.trueSink False @?= True,
    testCase "falseSink 1" $ F5.falseSink True @?= False,
    testCase "falseSink 2" $ F5.falseSink False @?= False,
    testCase "tripleAnd 1" $ F5.tripleAnd True True True @?= True,
    testCase "tripleAnd 2" $ F5.tripleAnd True True False @?= False,
    testCase "addToFirstList 1" $ F5.addToFirstList [1, 2] [7, 8] @?= ([8, 2], [8]),
    testCase "addToFirstList 2" $ F5.addToFirstList [34, 16, 14] [-13, 7, 2] @?= ([21, 16, 14], [7, 2])
  ]
