module Typeclass.Typeclasses1Test where

import Test.Tasty
import Test.Tasty.HUnit
import Typeclass.Typeclasses1 (InterestRate (..), Occupation (..), Person (..))
import qualified Typeclass.Typeclasses1 as T1

test_Typeclasses1 :: [TestTree]
test_Typeclasses1 =
  [ testCase "Occupation 1" $ T1.equalMessage Lawyer Lawyer @?= "Objects are both 'Lawyer'!",
    testCase "Occupation 2" $ T1.equalMessage Programmer Engineer @?= "Objects 'Programmer' and 'Engineer' are not equal!",
    testCase "Person 1" $
      T1.equalMessage (Adult "John" "Smith" 32 Doctor) (Adult "Jane" "Smith" 31 Engineer)
        @?= "Objects 'Adult \"John\" \"Smith\" 32 Doctor' and 'Adult \"Jane\" \"Smith\" 31 Engineer' are not equal!",
    testCase "Person 2" $
      T1.equalMessage (Adult "John" "Smith" 32 Doctor) (Child "Chris" 12 7)
        @?= "Objects 'Adult \"John\" \"Smith\" 32 Doctor' and 'Child \"Chris\" 12 7' are not equal!",
    testCase "Person 3" $
      T1.equalMessage (Child "Chris" 12 7) (Child "Chris" 12 7)
        @?= "Objects are both 'Child \"Chris\" 12 7'!",
    testCase "Interest Rate 1" $ T1.equalMessage (InterestRate 0.5) (InterestRate 0.5) @?= "Objects are both 'InterestRate 0.5'!",
    testCase "Interest Rate 2" $
      T1.equalMessage (InterestRate 0.7) (InterestRate 0.5)
        @?= "Objects 'InterestRate 0.7' and 'InterestRate 0.5' are not equal!"
  ]
