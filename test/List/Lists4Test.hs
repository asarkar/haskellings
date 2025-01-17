module List.Lists4Test where

import qualified List.Lists4 as L4
import Test.Tasty
import Test.Tasty.HUnit

test_List4 :: [TestTree]
test_List4 =
  [ testCase "sumProductDifference 1" $ L4.sumProductDifference [] [] @?= [],
    testCase "sumProductDifference 2" $ L4.sumProductDifference [1, 2] [] @?= [],
    testCase "sumProductDifference 3" $ L4.sumProductDifference [1] [3, 4] @?= [(4, 3, -2)],
    testCase "sumProductDifference 4" $
      L4.sumProductDifference [1, 2, 3, 4] [5, 6, 7, 8]
        @?= [(6, 5, -4), (8, 12, -4), (10, 21, -4), (12, 32, -4)],
    testCase "sumProductDifference 5" $
      L4.sumProductDifference [13, 10, 11] [5, 6, 7, 8]
        @?= [(18, 65, 8), (16, 60, 4), (18, 77, 4)],
    testCase "addIndices 1" $ L4.addIndices [] @?= [],
    testCase "addIndices 2" $ L4.addIndices ["Hello"] @?= ["Hello0"],
    testCase "addIndices 3" $ L4.addIndices ["Hello", "Goodbye"] @?= ["Hello0", "Goodbye1"],
    testCase "addIndices 4" $ L4.addIndices ["1", "0", "2"] @?= ["10", "01", "22"]
  ]
