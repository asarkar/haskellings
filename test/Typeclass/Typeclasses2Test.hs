module Typeclass.Typeclasses2Test where

import Test.Tasty
import Test.Tasty.HUnit
import Typeclass.Typeclasses2 (InterestRate (..), Occupation (..), Person (..))

test_Typeclasses2 :: [TestTree]
test_Typeclasses2 =
  [ testCase "Show Occupation" $ map show [Lawyer, Programmer] @?= ["lawyer", "programmer"],
    testCase "Eq Occupation" $ map (uncurry (==)) [(Lawyer, Engineer), (Engineer, Engineer), (Doctor, Doctor)] @?= [False, True, True],
    testCase "Show Person 1" $ show (Adult "John" "Smith" 32 Doctor) @?= "John Smith is 32 years old",
    testCase "Show Person 2" $ show (Adult "Jane" "Smith" 31 Teacher) @?= "Jane Smith is 31 years old",
    testCase "Show Person 3" $ show (Child "Chris" 12 7) @?= "Chris is 12 years old",
    testCase "Eq Person 1" $ Adult "John" "Smith" 32 Doctor == Adult "John" "Smith" 35 Engineer @?= True,
    testCase "Eq Person 2" $ Adult "John" "Smith" 32 Doctor == Adult "John" "Adams" 32 Doctor @?= False,
    testCase "Eq Person 3" $ Child "Chris" 12 7 == Adult "Chris" "Smith" 35 Teacher @?= False,
    testCase "Eq Person 4" $ Child "Chris" 12 7 == Child "Chris" 11 6 @?= True,
    testCase "Eq Person 5" $ Child "Chris" 12 7 == Child "Christine" 12 7 @?= False,
    testCase "Show Interest Rate" $ map show [InterestRate 0.5, InterestRate 0.8] @?= ["0.5", "0.8"],
    testCase "Eq Interest Rate" $ map (uncurry (==)) [(InterestRate 0.3, InterestRate 0.3), (InterestRate 0.3, InterestRate 0.5)] @?= [True, False]
  ]
