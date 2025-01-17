module List.Lists2Test where

import qualified List.Lists2 as L2
import Test.Tasty
import Test.Tasty.HUnit

test_List2 :: [TestTree]
test_List2 =
  [ testCase "countdownBy5 1" $ L2.countdownBy5 4 @?= [],
    testCase "countdownBy5 2" $ L2.countdownBy5 5 @?= [5, 0],
    testCase "countdownBy5 3" $ L2.countdownBy5 7 @?= [7, 2],
    testCase "countdownBy5 4" $
      L2.countdownBy5 95
        @?= [95, 90, 85, 80, 75, 70, 65, 60, 55, 50, 45, 40, 35, 30, 25, 20, 15, 10, 5, 0],
    testCase "give100 1" $ L2.give100 2 @?= replicate 100 2,
    testCase "give100 2" $ L2.give100 34 @?= replicate 100 34
  ]
