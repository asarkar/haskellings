module Monad.Transformers1Test where

import qualified Control.Monad.State as S
import Monad.Ops (Op (..))
import qualified Monad.Transformers1 as T1
import Test.Tasty
import Test.Tasty.HUnit

opList1 :: [Op]
opList1 = [Add 5.0, Subtract 2.0, Multiply 3.0, Divide 2.0]

opList2 :: [Op]
opList2 = [Multiply 2.0, Sqrt]

test_Transformers1 :: [TestTree]
test_Transformers1 =
  [ testCase "applyOperations 1" $ S.runStateT (T1.applyOperations [] 5.0) 0 >>= (@?= (5.0, 0)),
    testCase "applyOperations 2" $ S.runStateT (T1.applyOperations [Add 1.0] 5.0) 0 >>= (@?= (6.0, 1)),
    testCase "applyOperations 3" $ S.runStateT (T1.applyOperations [Subtract 3.0] 8.0) 0 >>= (@?= (5.0, 2)),
    testCase "applyOperations 4" $ S.runStateT (T1.applyOperations opList1 13.0) 0 >>= (@?= (24.0, 18)),
    testCase "applyOperations 5" $ S.runStateT (T1.applyOperations opList2 8.0) 0 >>= (@?= (4.0, 25))
  ]
