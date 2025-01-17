module Recursion.Recursion3Test where

import qualified Recursion.Recursion3 as R3
import Test.Tasty
import Test.Tasty.HUnit

test_Recursion3 :: [TestTree]
test_Recursion3 =
  [ testCase "Reverse 1" $ R3.reverseList [] @?= ([] :: [Int]),
    testCase "Reverse 2" $ R3.reverseList [1] @?= [1],
    testCase "Reverse 3" $ R3.reverseList [1, 2, 3, 4, 5, 6] @?= [6, 5, 4, 3, 2, 1],
    testCase "Reverse 4" $ R3.reverseList ["Hello", "Goodbye", "There"] @?= ["There", "Goodbye", "Hello"],
    testCase "evenOdds 1" $ R3.evenOdds [] @?= ([], [] :: [Int]),
    testCase "evenOdds 2" $ R3.evenOdds [1] @?= ([], [1]),
    testCase "evenOdds 3" $ R3.evenOdds [1, 2, 3] @?= ([2], [1, 3]),
    testCase "evenOdds 4" $ R3.evenOdds [1, 2, 3, 4] @?= ([2, 4], [1, 3]),
    testCase "evenOdds 5" $ R3.evenOdds [1, 2, 3, 4, 5, 6, 7] @?= ([2, 4, 6], [1, 3, 5, 7])
  ]
