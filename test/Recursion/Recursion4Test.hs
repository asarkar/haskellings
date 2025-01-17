module Recursion.Recursion4Test where

import Recursion.Recursion4 (BinaryTree (..))
import qualified Recursion.Recursion4 as R4
import Test.Tasty
import Test.Tasty.HUnit

tree1 :: BinaryTree
tree1 =
  ValueNode
    8
    (ValueNode 3 (ValueNode 1 EmptyNode EmptyNode) (ValueNode 6 (ValueNode 4 EmptyNode EmptyNode) (ValueNode 7 EmptyNode EmptyNode)))
    (ValueNode 10 EmptyNode (ValueNode 14 (ValueNode 13 EmptyNode EmptyNode) EmptyNode))

tree2 :: BinaryTree
tree2 =
  ValueNode
    17
    (ValueNode 3 (ValueNode 1 EmptyNode EmptyNode) (ValueNode 6 (ValueNode 4 EmptyNode EmptyNode) (ValueNode 24 EmptyNode EmptyNode)))
    (ValueNode 10 EmptyNode (ValueNode 14 (ValueNode 13 EmptyNode EmptyNode) EmptyNode))

test_Recursion4 :: [TestTree]
test_Recursion4 =
  [ testCase "sumTree 1" $ R4.sumTree EmptyNode @?= 0,
    testCase "sumTree 2" $ R4.sumTree (ValueNode 4 EmptyNode EmptyNode) @?= 4,
    testCase "sumTree 3" $ R4.sumTree (ValueNode 4 (ValueNode 5 EmptyNode EmptyNode) EmptyNode) @?= 9,
    testCase "sumTree 4" $ R4.sumTree tree1 @?= 66,
    testCase "sumTree 5" $ R4.sumTree tree2 @?= 92,
    testCase "isValidBst 1" $ R4.isValidBst EmptyNode @?= True,
    testCase "isValidBst 2" $ R4.isValidBst (ValueNode 4 EmptyNode EmptyNode) @?= True,
    testCase "isValidBst 3" $ R4.isValidBst (ValueNode 4 (ValueNode 5 EmptyNode EmptyNode) EmptyNode) @?= False,
    testCase "isValidBst 4" $ R4.isValidBst tree1 @?= True,
    testCase "isValidBst 5" $ R4.isValidBst tree2 @?= False
  ]
