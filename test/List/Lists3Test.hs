module List.Lists3Test where

import qualified List.Lists3 as L3
import Test.Tasty
import Test.Tasty.HUnit

test_Lists3 :: [TestTree]
test_Lists3 =
  [ testCase "addMod3Is2 1" $ L3.addMod3Is2 [] @?= [],
    testCase "addMod3Is2 2" $ L3.addMod3Is2 [2] @?= [5],
    testCase "addMod3Is2 3" $ L3.addMod3Is2 [2, 4, 5, 8, 9] @?= [5, 8, 11],
    testCase "Pairwise Products 1" $ L3.smallPairwiseProducts [] [] @?= [],
    testCase "Pairwise Products 2" $ L3.smallPairwiseProducts [] [1] @?= [],
    testCase "Pairwise Products 3" $ L3.smallPairwiseProducts [2] [] @?= [],
    testCase "Pairwise Products 4" $ L3.smallPairwiseProducts [2] [1] @?= [2],
    testCase "Pairwise Products 5" $ L3.smallPairwiseProducts [2] [29] @?= [],
    testCase "Pairwise Products 6" $
      L3.smallPairwiseProducts [1, 2, 3] [1, 2, 3]
        @?= [1, 2, 3, 2, 4, 6, 3, 6, 9],
    testCase "Pairwise Products 7" $
      L3.smallPairwiseProducts [1, 2, 3] [27, 28, 29]
        @?= [27, 28, 54]
  ]
