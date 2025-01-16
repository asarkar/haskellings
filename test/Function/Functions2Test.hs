module Function.Functions2Test where

import qualified Function.Functions2 as F2
import Test.Tasty
import Test.Tasty.HUnit

test_Functions2 :: [TestTree]
test_Functions2 =
  [ testCase "Add Heads 1" $ F2.addHeads ([1, 2, 3], [4, 5, 6]) @?= (5, [2, 3], [5, 6]),
    testCase "Add Heads 2" $ F2.addHeads ([-1, 6, 9], [3, -3, -10]) @?= (2, [6, 9], [-3, -10]),
    testCase "Take Seconds 1" $ F2.takeSeconds (4, 5) (1, 2) (-1, -2) @?= (5, 2, -2),
    testCase "Take Seconds 2" $ F2.takeSeconds (100, 121) (-3, -4) (50, -67) @?= (121, -4, -67),
    testCase "Capitalize 1" $ F2.capitalize ('a', 'b') @?= ('A', 'B'),
    testCase "Capitalize 2" $ F2.capitalize ('f', 'q') @?= ('F', 'Q')
  ]
