module Evaluator
  ( Environment
  , eval
  , evalBinOp
  , evalUnaryOp
  , evalFunction
  ) where

import AST
import Data.Map (Map)
import qualified Data.Map as Map

type Environment = Map String Double

-- 評価関数
eval :: Environment -> Expr -> (Double, Environment)
eval env (Num n) = (n, env)
eval env (Var name) = 
  case Map.lookup name env of
    Just val -> (val, env)
    Nothing  -> error $ "未定義の変数: " ++ name
eval env (Binary op left right) = 
  let (leftVal, env1) = eval env left
      (rightVal, env2) = eval env1 right
  in (evalBinOp op leftVal rightVal, env2)
eval env (Unary op expr) =
  let (val, env1) = eval env expr
  in (evalUnaryOp op val, env1)
eval env (Call name args) =
  let argVals = map (fst . eval env) args
  in (evalFunction name argVals, env)
eval env (Assign name expr) =
  let (val, env1) = eval env expr
  in (val, Map.insert name val env1)

-- 二項演算の評価
evalBinOp :: BinOp -> Double -> Double -> Double
evalBinOp Add a b = a + b
evalBinOp Sub a b = a - b
evalBinOp Mul a b = a * b
evalBinOp Div a b = a / b
evalBinOp Pow a b = a ** b

-- 単項演算の評価
evalUnaryOp :: UnaryOp -> Double -> Double
evalUnaryOp Neg a = -a
evalUnaryOp Pos a = a

-- 組み込み関数の評価
evalFunction :: String -> [Double] -> Double
evalFunction "sin" [x] = sin x
evalFunction "cos" [x] = cos x
evalFunction "tan" [x] = tan x
evalFunction "sqrt" [x] = sqrt x
evalFunction "log" [x] = log x
evalFunction name _ = error $ "未定義の関数: " ++ name 