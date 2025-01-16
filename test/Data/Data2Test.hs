module Data.Data2Test where

import Data.Data2 (Occupation (..), Person (..))
import qualified Data.Data2 as D2
import Test.Tasty
import Test.Tasty.HUnit

adult1 :: Person
adult1 = Adult "John" "Smith" 45 Lawyer

adult2 :: Person
adult2 = Adult "Jane" "Smith" 39 Engineer

child1 :: Person
child1 = Child "Christopher" 9 4

child2 :: Person
child2 = Child "Stephanie" 12 6

test_Data2 :: [TestTree]
test_Data2 =
  [ testCase "giveFullName 1" $ D2.giveFullName adult1 @?= "John Smith",
    testCase "giveFullName 2" $ D2.giveFullName adult2 @?= "Jane Smith",
    testCase "giveFullName 3" $ D2.giveFullName child1 @?= "Christopher",
    testCase "giveFullName 4" $ D2.giveFullName child2 @?= "Stephanie"
  ]
