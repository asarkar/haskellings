module Syntax.Syntax5Test where

import qualified Syntax.Syntax5 as S5
import Test.Tasty
import Test.Tasty.HUnit

test_Syntax5 :: [TestTree]
test_Syntax5 =
  [ testCase "sumPairProducts 1" $ S5.sumPairProducts (1, 2, 3, 4, 5, 6) @?= 175,
    testCase "sumPairProducts 2" $ S5.sumPairProducts (8, 2, -3, 4, -5, 7) @?= 1,
    testCase "sumTuples 1" $ S5.sumTuples (True, True, True) (1, 2, 3) (4, 5, 6) @?= 21,
    testCase "sumTuples 2" $ S5.sumTuples (True, False, True) (1, 2, 3) (4, 5, 6) @?= 14,
    testCase "sumTuples 3" $ S5.sumTuples (False, True, False) (1, 2, 3) (4, 5, 6) @?= 7
  ]
