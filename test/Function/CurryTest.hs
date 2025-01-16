module Function.CurryTest where

import Function.Curry as C
import Test.Tasty
import Test.Tasty.HUnit

-- test_curry :: TestTree
-- test_curry =
--   testGroup
--     "Curry"
--     [ testCase "Manhattan Distance 1" $ C.manhattanDistancePoint (4, 6) @?= 10,
--       testCase "Manhattan Distance 2" $ C.manhattanDistancePoint (2, -5) @?= 7,
--       testCase "Manhattan Distance 3" $ C.manhattanDistancePoint (-3, -9) @?= 12,
--       testCase "andCurried 1" $ C.andCurried True False @?= False,
--       testCase "andCurried 2" $ C.andCurried True True @?= True
--     ]
test_curry :: [TestTree]
test_curry =
  [ testCase "Manhattan Distance 1" $ C.manhattanDistancePoint (4, 6) @?= 10,
    testCase "Manhattan Distance 2" $ C.manhattanDistancePoint (2, -5) @?= 7,
    testCase "Manhattan Distance 3" $ C.manhattanDistancePoint (-3, -9) @?= 12,
    testCase "andCurried 1" $ C.andCurried True False @?= False,
    testCase "andCurried 2" $ C.andCurried True True @?= True
  ]
