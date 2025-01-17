module Typeclass.Typeclasses5Test where

import Test.Tasty
import Test.Tasty.HUnit
import Typeclass.Typeclasses5 (Person (..), Point3 (..))
import qualified Typeclass.Typeclasses5 as T5

test_Typeclasses5 :: [TestTree]
test_Typeclasses5 =
  [ testCase "Log Object 1" $ T5.logObject (Person "John" "Smith" 32) "Test.hs" @?= "Produced 'Person \"John\" \"Smith\" 32' from file Test.hs",
    testCase "Log Object 2" $ T5.logObject (Person "Jane" "Doe" 31) "Input.txt" @?= "Produced 'Person \"Jane\" \"Doe\" 31' from file Input.txt",
    testCase "Log Object 3" $ T5.logObject (Point3 3 4 5) "Test.hs" @?= "Calculated 'Point3 3 4 5' from input file Test.hs",
    testCase "Log Object 4" $ T5.logObject (Point3 6 8 10) "Input.csv" @?= "Calculated 'Point3 6 8 10' from input file Input.csv",
    testCase "Compare From Entry 1" $
      T5.compareFromEntry "Person \"John\" \"Smith\" 32" (Person "John" "Smith" 32)
        @?= (True, "Found entered Person object equivalent."),
    testCase "Compare From Entry 2" $
      T5.compareFromEntry "Person \"John\" \"Smith\" 32" (Person "Jane" "Smith" 35)
        @?= (False, "Found entered Person object does not match."),
    testCase "Compare From Entry 3" $
      T5.compareFromEntry "Point3 3 4 5" (Point3 3 4 5)
        @?= (True, "New Point calculation matches."),
    testCase "Compare From Entry 4" $
      T5.compareFromEntry "Point3 3 4 5" (Point3 6 8 10)
        @?= (False, "New Point calculation does not match previous."),
    testCase "Compare and Print 1" $
      T5.compareAndPrint "Person \"John\" \"Smith\" 32" (Person "John" "Smith" 32)
        @?= (True, "'Person \"John\" \"Smith\" 32' vs. 'Person \"John\" \"Smith\" 32'"),
    testCase "Compare and Print 2" $
      T5.compareAndPrint "Person \"John\" \"Smith\" 32" (Person "Jane" "Smith" 31)
        @?= (False, "'Person \"John\" \"Smith\" 32' vs. 'Person \"Jane\" \"Smith\" 31'"),
    testCase "Compare and Print 3" $
      T5.compareAndPrint "Point3 3 4 5" (Point3 3 4 5)
        @?= (True, "'Point3 3 4 5' vs. 'Point3 3 4 5'"),
    testCase "Compare and Print 4" $
      T5.compareAndPrint "Point3 3 4 5" (Point3 6 8 10)
        @?= (False, "'Point3 3 4 5' vs. 'Point3 6 8 10'")
  ]
