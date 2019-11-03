import System.Environment as E
import System.IO
import Text.Printf

type Operator = Char
type Token = Either Operator Int
data Expr = UExpr Int | RExpr Operator Expr Expr deriving Show

getInt :: Either a b -> b
getInt (Right x) = x
getOp :: Either a b -> a
getOp (Left x) = x

lt :: Operator -> Operator -> Bool
lt '+' '*' = True
lt _ _     = False

build_expr_tree :: String -> Expr
build_expr_tree = build_from_toks . (map parse_tok) . words
  where build_from_toks (t:[]) = UExpr (getInt t)
        build_from_toks ts     = let
          (e, ts2) = build_init_state ts
          in fst $ append_next e ts2

parse_tok :: String -> Token
parse_tok s | elem (head s) "*+" = Left $ head s
            | otherwise          = Right (read s :: Int)

build_init_state :: [Token] -> (Expr, [Token])
build_init_state (o1:op:o2:ts) = (RExpr (getOp op) (UExpr $ getInt o1) (UExpr $ getInt o2), ts)

append_next :: Expr -> [Token] -> (Expr, [Token])
append_next e []        = (e, [])
append_next e (op:x:ts) = append_next (append_single e (getInt x) (getOp op)) ts
  where append_single e0@(RExpr op1 e1 e2) i op2 
          | op1 `lt` op2 = RExpr op1 e1 (append_single e2 i op2)
          | otherwise    = RExpr op2 e0 (UExpr i)
        append_single e i op2 = RExpr op2 e (UExpr i)

showParens :: Expr -> String
showParens (UExpr i)         = show i
showParens (RExpr op1 e1 e2) = printf "(%s %c %s)" (showParens e1) op1 (showParens e2)

main = do
  (expr:_) <- getArgs
  print $ map parse_tok (words expr)
  print $ build_expr_tree expr
  print $ showParens $ build_expr_tree expr

