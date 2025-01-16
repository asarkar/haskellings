module Syntax.Syntax6Test where

import Syntax.Syntax6 as S6
import Test.Tasty
import Test.Tasty.HUnit

test_Syntax6 :: [TestTree]
test_Syntax6 =
  [ testCase "sumPairProducts 1" $ S6.sumPairProducts (1, 2, 3, 4, 5, 6) @?= 175,
    testCase "sumPairProducts 2" $ S6.sumPairProducts (8, 2, -3, 4, -5, 7) @?= 1,
    testCase "sumTuples 1" $ S6.sumTuples (True, True, True) (1, 2, 3) (4, 5, 6) @?= 21,
    testCase "sumTuples 2" $ S6.sumTuples (True, False, True) (1, 2, 3) (4, 5, 6) @?= 14,
    testCase "sumTuples 3" $ S6.sumTuples (False, True, False) (1, 2, 3) (4, 5, 6) @?= 7
  ]
