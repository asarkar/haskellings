module Monad.Monads4Test where

import qualified Control.Monad.Writer as W
import qualified Monad.Monads4 as M4
import Monad.Ops (IntAdd (..), Op (..))
import Test.Tasty
import Test.Tasty.HUnit

opList1 :: [Op]
opList1 = [Add 5.0, Subtract 2.0, Multiply 3.0, Divide 2.0]

opList2 :: [Op]
opList2 = [Multiply 2.0, Sqrt]

test_Monads4 :: [TestTree]
test_Monads4 =
  [ testCase "applyOpCount 1" $ W.runWriter (M4.applyOpCount (Add 1) 5.0) @?= (6.0, IntAdd 1),
    testCase "applyOpCount 2" $ W.runWriter (M4.applyOpCount (Multiply 2) 5.0) @?= (10.0, IntAdd 5),
    testCase "applyOpCount 3" $ W.runWriter (M4.applyOpCount (Divide 2) 6.0) @?= (3.0, IntAdd 10),
    testCase "applyOpCount 4" $ W.runWriter (M4.applyOpCount Sqrt 4.0) @?= (2.0, IntAdd 20),
    testCase "applyAndCountOperations 1" $ M4.applyAndCountOperations [] 5.0 @?= (5.0, IntAdd 0),
    testCase "applyAndCountOperations 2" $ M4.applyAndCountOperations [Add 1.0] 5.0 @?= (6.0, IntAdd 1),
    testCase "applyAndCountOperations 3" $ M4.applyAndCountOperations [Subtract 3.0] 8.0 @?= (5.0, IntAdd 2),
    testCase "applyAndCountOperations 4" $ M4.applyAndCountOperations opList1 13.0 @?= (24.0, IntAdd 18),
    testCase "applyAndCountOperations 5" $ M4.applyAndCountOperations opList2 8.0 @?= (4.0, IntAdd 25),
    testCase "applyOpLog 1" $ W.runWriter (M4.applyOpLog (Add 1) 5.0) @?= (6.0, ["+ 1.0"]),
    testCase "applyOpLog 2" $ W.runWriter (M4.applyOpLog (Multiply 2) 5.0) @?= (10.0, ["* 2.0"]),
    testCase "applyOpLog 3" $ W.runWriter (M4.applyOpLog (Divide 2) 6.0) @?= (3.0, ["/ 2.0"]),
    testCase "applyOpLog 4" $ W.runWriter (M4.applyOpLog Sqrt 4.0) @?= (2.0, ["√"]),
    testCase "applyAndLogOperations 1" $ M4.applyAndLogOperations [] 5.0 @?= (5.0, []),
    testCase "applyAndLogOperations 2" $ M4.applyAndLogOperations [Add 1.0] 5.0 @?= (6.0, ["+ 1.0"]),
    testCase "applyAndLogOperations 3" $ M4.applyAndLogOperations [Subtract 3.0] 8.0 @?= (5.0, ["- 3.0"]),
    testCase "applyAndLogOperations 4" $
      M4.applyAndLogOperations opList1 13.0
        @?= (24.0, ["+ 5.0", "- 2.0", "* 3.0", "/ 2.0"]),
    testCase "applyAndLogOperations 5" $
      M4.applyAndLogOperations opList2 8.0
        @?= (4.0, ["* 2.0", "√"])
  ]
