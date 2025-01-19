module Monad.Ops where

import qualified Text.ParserCombinators.ReadP as RP
import Text.ParserCombinators.ReadPrec (ReadPrec)
import qualified Text.ParserCombinators.ReadPrec as RPrec
import qualified Text.Read as R

-- The original course duplicates these
-- types in multiple modules.

newtype IntAdd = IntAdd Int
  deriving stock (Show, Eq)

-- Here we have several operations we might apply on an input Double.
-- Each of them has a particular cost associated with them.
data Op = Add Double | Subtract Double | Multiply Double | Divide Double | Sqrt
  deriving stock (Eq)

-- The Read and Show instances differ from those
-- in the original course. Instead of "Add 5.0",
-- we choose the more natural "+ 5.0".

instance Read Op where
  readPrec =
    R.parens
      ( RPrec.prec RPrec.minPrec $ do
          c <- RPrec.get
          if c == '√'
            then return Sqrt
            else opThenNum c
      )
  readListPrec = R.readListPrecDefault

opThenNum :: Char -> ReadPrec Op
opThenNum c =
  case c of
    '+' -> Add <$> readD
    '-' -> Subtract <$> readD
    '*' -> Multiply <$> readD
    '/' -> Divide <$> readD
    _ -> R.pfail
  where
    readD = do
      n <- num
      case R.readMaybe n of
        Just d -> return d
        _ -> R.pfail
      where
        num = R.lift (RP.many1 RP.get)

-- skipSpacesThenP :: ReadPrec a -> ReadPrec a
-- skipSpacesThenP m =
--   do
--     s <- R.look
--     skip s
--   where
--     skip (c : s) | C.isSpace c = R.get *> skip s
--     skip _ = m

instance Show Op where
  show op = case op of
    Add x -> "+ " ++ f x
    Subtract x -> "- " ++ f x
    Multiply x -> "* " ++ f x
    Divide x -> "/ " ++ f x
    Sqrt -> "√"
    where
      f x = if x >= 0 then show x else "(" ++ show x ++ ")"

opCost :: Op -> IntAdd
opCost op = IntAdd $ case op of
  Add _ -> 1
  Subtract _ -> 2
  Multiply _ -> 5
  Divide _ -> 10
  Sqrt -> 20

applyOp :: Op -> Double -> Double
applyOp op y = case op of
  Add x -> x + y
  Subtract x -> y - x
  Multiply x -> x * y
  Divide x -> y / x
  Sqrt -> if y < 0 then y else sqrt y
