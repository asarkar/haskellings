module Monad.Monads5Test where

import qualified Control.Monad.State as S
import qualified Monad.Monads5 as M5
import Monad.Ops (IntAdd (..), Op (..))
import Test.Tasty
import Test.Tasty.HUnit

opList1 :: [Op]
opList1 = [Add 5.0, Subtract 2.0, Multiply 3.0, Divide 2.0]

opList2 :: [Op]
opList2 = [Multiply 2.0, Sqrt]

test_Monads5 :: [TestTree]
test_Monads5 =
  [ testCase "applyOpCount 1" $ S.runState (M5.applyOpCount (Add 1) 5.0) (IntAdd 0) @?= (6.0, IntAdd 1),
    testCase "applyOpCount 2" $ S.runState (M5.applyOpCount (Multiply 2) 5.0) (IntAdd 0) @?= (10.0, IntAdd 5),
    testCase "applyOpCount 3" $ S.runState (M5.applyOpCount (Divide 2) 6.0) (IntAdd 0) @?= (3.0, IntAdd 10),
    testCase "applyOpCount 4" $ S.runState (M5.applyOpCount Sqrt 4.0) (IntAdd 0) @?= (2.0, IntAdd 20),
    testCase "applyAndCountOperations 1" $ M5.applyAndCountOperations [] 5.0 @?= (5.0, IntAdd 0),
    testCase "applyAndCountOperations 2" $ M5.applyAndCountOperations [Add 1.0] 5.0 @?= (6.0, IntAdd 1),
    testCase "applyAndCountOperations 3" $ M5.applyAndCountOperations [Subtract 3.0] 8.0 @?= (5.0, IntAdd 2),
    testCase "applyAndCountOperations 4" $ M5.applyAndCountOperations opList1 13.0 @?= (24.0, IntAdd 18),
    testCase "applyAndCountOperations 5" $ M5.applyAndCountOperations opList2 8.0 @?= (4.0, IntAdd 25),
    testCase "applyOpLog 1" $ S.runState (M5.applyOpLog (Add 1) 5.0) [] @?= (6.0, ["+ 1.0"]),
    testCase "applyOpLog 2" $ S.runState (M5.applyOpLog (Multiply 2) 5.0) [] @?= (10.0, ["* 2.0"]),
    testCase "applyOpLog 3" $ S.runState (M5.applyOpLog (Divide 2) 6.0) [] @?= (3.0, ["/ 2.0"]),
    testCase "applyOpLog 4" $ S.runState (M5.applyOpLog Sqrt 4.0) [] @?= (2.0, ["√"]),
    testCase "applyAndLogOperations 1" $ M5.applyAndLogOperations [] 5.0 @?= (5.0, []),
    testCase "applyAndLogOperations 2" $ M5.applyAndLogOperations [Add 1.0] 5.0 @?= (6.0, ["+ 1.0"]),
    testCase "applyAndLogOperations 3" $ M5.applyAndLogOperations [Subtract 3.0] 8.0 @?= (5.0, ["- 3.0"]),
    testCase "applyAndLogOperations 4" $
      M5.applyAndLogOperations opList1 13.0
        @?= (24.0, ["+ 5.0", "- 2.0", "* 3.0", "/ 2.0"]),
    testCase "applyAndLogOperations 5" $
      M5.applyAndLogOperations opList2 8.0
        @?= (4.0, ["* 2.0", "√"]),
    testCase "applyOpSimple 1" $ S.execState (M5.applyOpSimple (Add 1)) 5.0 @?= 6.0,
    testCase "applyOpSimple 2" $ S.execState (M5.applyOpSimple (Multiply 2)) 5.0 @?= 10.0,
    testCase "applyOpSimple 3" $ S.execState (M5.applyOpSimple (Divide 2)) 6.0 @?= 3.0,
    testCase "applyOpSimple 4" $ S.execState (M5.applyOpSimple Sqrt) 4.0 @?= 2.0,
    testCase "applySimpleOperations 1" $ M5.applySimpleOperations [] 5.0 @?= 5.0,
    testCase "applySimpleOperations 2" $ M5.applySimpleOperations [Add 1.0] 5.0 @?= 6.0,
    testCase "applySimpleOperations 3" $ M5.applySimpleOperations [Subtract 3.0] 8.0 @?= 5.0,
    testCase "applySimpleOperations 4" $ M5.applySimpleOperations opList1 13.0 @?= 24.0,
    testCase "applySimpleOperations 5" $ M5.applySimpleOperations opList2 8.0 @?= 4.0
  ]
