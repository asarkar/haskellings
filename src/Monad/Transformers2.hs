{-# LANGUAGE DerivingStrategies #-}

module Monad.Transformers2 where

import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Control.Monad.Trans.Maybe as Mb
import qualified Text.Read as R

{-

- Since 'Maybe' is a monad as well, it also has a monad transformer 'MaybeT'.
  There's no 'runMaybe' function though. You just use 'MaybeT' as a constructor
  aroud a function in the underlying monad.

newtype MaybeT m a = MaybeT
  { runMaybeT :: m (Maybe a) }

maybeAction :: MaybeT IO String
maybeAction = MaybeT $ do
  input <- getLine    -- Normal IO monad actions!
  return (Just input) -- Must return a value as 'Maybe'

- We can see that 'runMaybeT' is the name of the 'field', so we can
  also use it as a function

callFromIO :: IO String
callFromIO = do
  result <- runMaybeT maybeAction
  case result of
    Nothing -> return "Default"
    Just s -> return s

- The 'MaybeT' transformer retains the semantics of the normal 'Maybe' monad.
  If one operation returns 'Nothing', the computation stops!

-}

data User = User
  { email :: String,
    password :: String,
    age :: Int
  }
  deriving stock (Show)

-- TODO:
--
-- Fill in these functions to populate a user with the validated information!
-- You should print the given prompt and then use 'getLine' to retrieve the
-- input, applying the specified validation and return 'Nothing' if it
-- doesn't pass.

-- Must contain the '@' and '.' characters.
-- Prompt: "Please enter your email"
readEmail :: MaybeT IO String
readEmail = MaybeT $ do
  putStrLn "Please enter your email."
  input <- getLine -- Normal IO monad actions!
  if '@' `elem` input && '.' `elem` input
    then return (Just input)
    else return Nothing

-- Must be at least 8 characters
-- Prompt: "Please enter your password"
readPassword :: MaybeT IO String
readPassword = MaybeT $ do
  putStrLn "Please enter your password."
  input <- getLine -- Normal IO monad actions!
  if length input >= 8
    then return (Just input)
    else return Nothing

-- Should pass 'readMaybe' for an Int
-- Prompt: "Please enter your age"
readAge :: MaybeT IO Int
readAge = MaybeT $ do
  putStrLn "Please enter your age."
  R.readMaybe <$> getLine

-- Apply the functions above to produce a 'User'
readUser :: MaybeT IO User
readUser = do
  email' <- readEmail
  password' <- readPassword
  User email' password' <$> readAge

-- Call your 'readUser' function on two different users,
-- printing the results each time.
main :: IO ()
main = Mb.runMaybeT readUser >>= print

{-

Sample Input:
stephanie@test.com
password
31
john@test.com
short

Sample Output:
Please enter your email.
Please enter your password.
Please enter your age.
Just (User {email = "stephanie@test.com", password = "password", age = 31})
Please enter your email.
Please enter your password.
Nothing

-}
