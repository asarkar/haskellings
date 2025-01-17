module Syntax.Syntax4Test where

import qualified Syntax.Syntax4 as S4
import Test.Tasty
import Test.Tasty.HUnit

test_Syntax4 :: [TestTree]
test_Syntax4 =
  [ testCase "evalList 1" $ S4.evalList True [] @?= 0,
    testCase "evalList 2" $ S4.evalList True [5] @?= 1,
    testCase "evalList 3" $ S4.evalList True [6, 7] @?= 2,
    testCase "evalList 4" $ S4.evalList True [14, 15, 16, 18, 19, 20] @?= 4,
    testCase "evalList 5" $ S4.evalList False [] @?= 0,
    testCase "evalList 6" $ S4.evalList False [5] @?= 5,
    testCase "evalList 7" $ S4.evalList False [6, 7] @?= 13,
    testCase "evalList 8" $ S4.evalList False [14, 15, 16, 18, 19, 20] @?= 10
  ]
