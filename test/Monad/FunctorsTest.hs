module Monad.FunctorsTest where

import Monad.Functors (Metrics (..))
import qualified Monad.Functors as F
import Test.Tasty
import Test.Tasty.HUnit

m1 :: Metrics Double
m1 = Metrics [3.0, 6.0] 4.5 6.0 3.0 Nothing

m2 :: Metrics Double
m2 = Metrics [1.0, 1.0, 5.0, 0.5] 1.875 5.0 0.5 (Just 1.0)

test_Functors :: [TestTree]
test_Functors =
  [ testCase "multiplySqrtDouble 1" $ F.multiplySqrtDouble (-2.0) (-8.0) @?= Just 8.0,
    testCase "multiplySqrtDouble 2" $ F.multiplySqrtDouble (-2.0) 8.0 @?= Nothing,
    testCase "multiplySqrtDouble 3" $ F.multiplySqrtDouble 3.0 3.0 @?= Just 6.0,
    testCase "multiplySqrtDouble 4" $ F.multiplySqrtDouble (-3.0) 0 @?= Just 0.0,
    testCase "Metrics Functor 1" $ (* 3) <$> m1 @?= Metrics [9.0, 18.0] 13.5 18.0 9.0 Nothing,
    testCase "Metrics Functor 2" $ (* 3) <$> m2 @?= Metrics [3.0, 3.0, 15.0, 1.5] 5.625 15.0 1.5 (Just 3.0),
    testCase "Double Metrics 1" $ F.doubleMetrics m1 @?= Metrics [6.0, 12.0] 9.0 12.0 6.0 Nothing,
    testCase "Double Metrics 2" $ F.doubleMetrics m2 @?= Metrics [2.0, 2.0, 10.0, 1.0] 3.75 10.0 1.0 (Just 2.0)
  ]
