module List.Lists1Test where

import qualified List.Lists1 as L1
import Test.Tasty
import Test.Tasty.HUnit

test_Lists1 :: [TestTree]
test_Lists1 =
  [ testCase "addMod3Is2 1" $ L1.addMod3Is2 [] @?= [],
    testCase "addMod3Is2 2" $ L1.addMod3Is2 [2] @?= [5],
    testCase "addMod3Is2 3" $ L1.addMod3Is2 [2, 4, 5, 8, 9] @?= [5, 8, 11],
    testCase "sumList 1" $ L1.sumList [] @?= 0,
    testCase "sumList 2" $ L1.sumList [5, -3] @?= 2,
    testCase "sumList 3" $ L1.sumList [6, 8, 9, -2, 10] @?= 31,
    testCase "allTrue 1" $ L1.allTrue [] @?= True,
    testCase "allTrue 2" $ L1.allTrue [False] @?= False,
    testCase "allTrue 3" $ L1.allTrue [False, True, True] @?= False,
    testCase "allTrue 4" $ L1.allTrue [True, True, True, True] @?= True
  ]
