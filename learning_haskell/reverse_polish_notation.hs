import Data.Maybe
import System.Environment
import System.IO
import Text.Read

data MyNum = MyInt Int | MyFlt Float deriving Show

instance Num MyNum where
  (+) (MyInt x) (MyInt y) = MyInt (x + y)
  (+) (MyInt x) (MyFlt y) = MyFlt ((fromIntegral x) + y)
  (+) (MyFlt x) (MyInt y) = MyFlt (x + (fromIntegral y))
  (+) (MyFlt x) (MyFlt y) = MyFlt (x + y)
  (-) (MyInt x) (MyInt y) = MyInt (x - y)
  (-) (MyInt x) (MyFlt y) = MyFlt ((fromIntegral x) - y)
  (-) (MyFlt x) (MyInt y) = MyFlt (x - (fromIntegral y))
  (-) (MyFlt x) (MyFlt y) = MyFlt (x - y)
  (*) (MyInt x) (MyInt y) = MyInt (x * y)
  (*) (MyInt x) (MyFlt y) = MyFlt ((fromIntegral x) * y)
  (*) (MyFlt x) (MyInt y) = MyFlt (x * (fromIntegral y))
  (*) (MyFlt x) (MyFlt y) = MyFlt (x * y)
  -- we do not really care about these ones
  signum x = MyInt 0
  abs x = MyInt 0
  fromInteger i = MyInt 0


data Operator = Plus | Minus | Mult deriving (Eq, Show)
to_op :: Char -> Operator
to_op c = case c of
  '+' -> Plus
  '-' -> Minus
  '*' -> Mult
apply :: Num t => Operator -> t -> t -> t
apply op o1 o2 | op == Plus = o1 + o2
               | op == Minus = o1 - o2
               | op == Mult = o1 * o2

type Token = Either Operator MyNum

evaluate :: String -> MyNum
evaluate = eval_list . parse_str 

parse_str :: String -> [Token]
parse_str = (map parse_tok) . words

parse_tok :: String -> Token
parse_tok s | elem (head s) "+-*" = Left $ to_op (head s)
            | otherwise           = Right $ parse_num s

parse_num :: String -> MyNum
parse_num s = let 
  is_int = readMaybe s :: Maybe Int
  is_flt = readMaybe s :: Maybe Float
  in if is_int /= Nothing 
    then MyInt $ fromJust is_int
    else MyFlt $ fromJust is_flt

eval_list :: [Token] -> MyNum
eval_list ls = head $ foldl accumulate [] ls
  where accumulate a (Right n) = n:a
        accumulate a (Left o)  = let
          (n1:ns1) = a
          (n2:ns2) = ns1
          in (apply o n1 n2) : ns2

main = do
  (expr:_) <- getArgs
  putStrLn $ show $ evaluate expr

