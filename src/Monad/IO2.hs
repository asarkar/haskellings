{-# LANGUAGE LambdaCase #-}

module Monad.IO2 where

import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Control.Monad.Trans.Maybe as Mb
import qualified Text.Printf as P
import qualified Text.Read as R

{-

- Once we know how to print items to the terminal, we need to know how
  to retrieve a user's input. This is done with the 'getLine' function:

getLine :: IO String

main = do
  -- (String)    (IO String)
  usersInput  <- getLine

- When reading non-string data, you'll likely have to use 'read' or
  some other parsing function. However, 'read' by itself will through
  a runtime error if the input string doesn't match our type.

-- Can't read "Hello" as an Integer!
badRead :: Int
badRead = read "Hello"

- To fix this, use 'readMaybe'. Then you can provide a more appropriate
  failure mechanism.

readMaybe :: (Read a) => String -> Maybe a

-}

-- TODO:

-- Prompt the user to enter a number by printing "Please enter a number."
-- Then retrieve an integer entered by a user.
-- If it can be read as an integer properly, print "Received x" where x is the number.
-- Otherwise, print "Could not read that as an integer." and return 'Nothing'.
readSingleInteger :: IO (Maybe Int)
readSingleInteger = do
  putStrLn "Please enter a number."
  n <- (R.readMaybe <$> getLine) :: IO (Maybe Int)
  case n of
    Just x -> P.printf "Received %d.\n" x
    _ -> putStrLn "Could not read that as an integer."
  return n

add :: Int -> Int -> IO (Maybe Int)
add x y =
  Just (x + y)
    <$ P.printf "The sum of these is %d.\n" (x + y)

tryAdd :: MaybeT IO Int
tryAdd = do
  x <- MaybeT readSingleInteger
  y <- MaybeT readSingleInteger
  n <- MaybeT (add x y)
  z <- MaybeT readSingleInteger
  MaybeT (add n z)

-- Start by retrieving two numbers. If they both succeed, print their sum
-- with "The sum of these is x." Then read another number and print the
-- final sum in the same format.
-- If any input fails, instead print "Sum is not possible." and exit.
main :: IO ()
main =
  Mb.runMaybeT tryAdd >>= \case
    Nothing -> putStrLn "Sum is not possible."
    _ -> return ()

{-

Sample Input:
4
5
"Hello"

Sample Output:
Please enter a number.
Received 4
Please enter a number.
Received 5
The sum of these is 9.
Please enter a number.
Could not read that as an integer.
Sum is not possible.

-}
