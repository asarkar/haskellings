module Recursion.Recursion2Test where

import qualified Recursion.Recursion2 as R2
import Test.Tasty
import Test.Tasty.HUnit

test_Recursion2 :: [TestTree]
test_Recursion2 =
  [ testCase "addMod3Is2 1" $ R2.addMod3Is2 [] @?= [],
    testCase "addMod3Is2 2" $ R2.addMod3Is2 [2] @?= [5],
    testCase "addMod3Is2 3" $ R2.addMod3Is2 [2, 4, 5, 8, 9] @?= [5, 8, 11],
    testCase "Evens 1" $ R2.evens [] @?= [],
    testCase "Evens 2" $ R2.evens [1] @?= [],
    testCase "Evens 3" $ R2.evens [1, 2, 3] @?= [2],
    testCase "Evens 4" $ R2.evens [1, 2, 3, 4] @?= [2, 4],
    testCase "Evens 5" $ R2.evens [1, 2, 3, 4, 5, 6, 7] @?= [2, 4, 6]
  ]
