module Syntax.Syntax2Test where

import qualified Syntax.Syntax2 as S2
import Test.Tasty
import Test.Tasty.HUnit

test_Syntax2 :: [TestTree]
test_Syntax2 =
  [ testCase "countTrue 1" $ S2.countTrue True False False @?= 1,
    testCase "countTrue 2" $ S2.countTrue False False False @?= 0,
    testCase "countTrue 3" $ S2.countTrue True False True @?= 2,
    testCase "countTrue 4" $ S2.countTrue True True True @?= 3,
    testCase "numberString 1" $ S2.numberString 1 @?= "One",
    testCase "numberString 2" $ S2.numberString 4 @?= "Four",
    testCase "numberString 3" $ S2.numberString 6 @?= "Too many!"
  ]
