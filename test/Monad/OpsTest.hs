module Monad.OpsTest where

import qualified Control.Applicative as A
import Monad.Ops (Op (..))
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.Printf as P
import qualified Text.Read as R

opList1 :: [Op]
opList1 = A.liftA2 id [Add, Subtract, Multiply, Divide] [5.0, -2.0, 0]

opList2 :: [(String, Op)]
opList2 = [("+ ( - 2.0)", Add (-2.0)), ("√", Sqrt)]

test_Ops :: [TestTree]
test_Ops =
  ((\op -> testCase (P.printf "show . read \"%s\"" (show op)) $ R.readMaybe (show op) @?= Just op) <$> opList1)
    ++ ((\(s, op) -> testCase (P.printf "read \"%s\"" s) $ R.readMaybe s @?= Just op) <$> opList2)
