module Typeclass.Typeclasses4Test where

import Test.Tasty
import Test.Tasty.HUnit
import Typeclass.Typeclasses4 (Adult (..), Child (..))
import qualified Typeclass.Typeclasses4 as T4

test_Typeclasses4 :: [TestTree]
test_Typeclasses4 =
  [ testCase "Get Name 1" $ T4.getName (Adult "John" "Smith" 45) @?= "John Smith",
    testCase "Get Name 2" $ T4.getName (Child "Chris" 17 12) @?= "Chris",
    testCase "Greet 1" $ T4.greet (Adult "John" "Smith" 45) @?= "Hello there, John Smith",
    testCase "Greet 2" $ T4.greet (Child "Chris" 17 12) @?= "Hello there, Chris",
    testCase "Compare 1" $ T4.compareByName (Child "Chris" 17 12) (Adult "John" "Smith" 45) @?= LT,
    testCase "Compare 2" $ T4.compareByName (Adult "Timothy" "Winston" 43) (Child "Edward" 15 10) @?= GT
  ]
