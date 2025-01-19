module Monad.Monads3Test where

import qualified Control.Monad.Reader as R
import Monad.Monads3 (User (..))
import qualified Monad.Monads3 as M3
import Test.Tasty
import Test.Tasty.HUnit

u1 :: User
u1 = User "john@test.com" "password" "John Smith" 35 "John is a Doctor at the Hospital"

u2 :: User
u2 = User "kate@test.com" "abcdefgh" "Kate Smith" 32 "Kate writes code."

test_Monads3 :: [TestTree]
test_Monads3 =
  [ testCase "add2AndShowDouble 1" $ R.runReader M3.add2AndShowDouble 1 @?= "3,4",
    testCase "add2AndShowDouble 2" $ R.runReader M3.add2AndShowDouble 7 @?= "9,16",
    testCase "Validate Account 1" $ R.runReader (M3.validateAccount "john@test.com" "password") u1 @?= True,
    testCase "Validate Account 2" $ R.runReader (M3.validateAccount "john@test.com" "abcdefgh") u1 @?= False,
    testCase "Validate Account 3" $ R.runReader (M3.validateAccount "john@test.com" "password") u2 @?= False,
    testCase "Validate Account 4" $ R.runReader (M3.validateAccount "kate@test.com" "password") u2 @?= False,
    testCase "Validate Account 5" $ R.runReader (M3.validateAccount "kate@test.com" "abcdefgh") u2 @?= True,
    testCase "Validate Account 6" $ R.runReader (M3.validateAccount "kate@test.com" "abcdefgh") u1 @?= False,
    testCase "Display Profile 1" $ R.runReader M3.displayProfile u1 @?= ["Name: John Smith", "Age: 35", "Bio: John is a Doctor at the Hospital"],
    testCase "Display Profile 2" $ R.runReader M3.displayProfile u2 @?= ["Name: Kate Smith", "Age: 32", "Bio: Kate writes code."],
    testCase "Auth and Display Profile 1" $
      M3.authAndDisplayProfile u1 ("john@test.com", "password")
        @?= Just ["Name: John Smith", "Age: 35", "Bio: John is a Doctor at the Hospital"],
    testCase "Auth and Display Profile 2" $ M3.authAndDisplayProfile u1 ("john@test.com", "abcdefgh") @?= Nothing,
    testCase "Auth and Display Profile 3" $ M3.authAndDisplayProfile u2 ("john@test.com", "password") @?= Nothing,
    testCase "Auth and Display Profile 4" $ M3.authAndDisplayProfile u2 ("kate@test.com", "password") @?= Nothing,
    testCase "Auth and Display Profile 5" $
      M3.authAndDisplayProfile u2 ("kate@test.com", "abcdefgh")
        @?= Just ["Name: Kate Smith", "Age: 32", "Bio: Kate writes code."],
    testCase "Auth and Display Profile 6" $ M3.authAndDisplayProfile u1 ("kate@test.com", "abcdefgh") @?= Nothing
  ]
