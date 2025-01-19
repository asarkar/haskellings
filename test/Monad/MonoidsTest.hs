module Monad.MonoidsTest where

import qualified Data.Monoid as M
import Monad.Monoids (IntAdd (..), IntMultiply (..))
import qualified Monad.Monoids as MM
import Test.Tasty
import Test.Tasty.HUnit

test_Monoids :: [TestTree]
test_Monoids =
  [ testCase "IntAdd 1" $ IntAdd 5 <> IntAdd 6 @?= IntAdd 11,
    testCase "IntAdd 2" $ IntAdd 3 <> IntAdd (-11) @?= IntAdd (-8),
    testCase "IntAdd 3" $ (M.mempty :: IntAdd) @?= IntAdd 0,
    testCase "IntMultiply 1" $ IntMultiply 5 <> IntMultiply 6 @?= IntMultiply 30,
    testCase "IntMultiply 2" $ IntMultiply 3 <> IntMultiply (-11) @?= IntMultiply (-33),
    testCase "IntMultiply 3" $ (M.mempty :: IntMultiply) @?= IntMultiply 1,
    testCase "abba 1" $ MM.abba (IntAdd 5) (IntAdd 6) @?= IntAdd 22,
    testCase "abba 2" $ MM.abba (IntMultiply 5) (IntMultiply 6) @?= IntMultiply 900,
    testCase "abba 3" $ MM.abba [1, 2, 3] [4, 5, 6] @?= [1, 2, 3, 4, 5, 6, 4, 5, 6, 1, 2, 3],
    testCase "abba 4" $ MM.abba "Hello " "Goodbye " @?= "Hello Goodbye Goodbye Hello "
  ]
