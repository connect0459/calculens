module CalcuLens
  ( parseAndEval,
    parseExpr,
    eval,
    Expr (..),
    Environment,
    BinOp(..),
    UnaryOp(..)
  )
where

import AST (Expr(..), BinOp(..), UnaryOp(..))
import Evaluator (Environment, eval)
import Parser (parseExpr)

-- パースして評価する便利関数
parseAndEval :: Environment -> String -> Either String (Double, Environment)
parseAndEval env input =
  case parseExpr input of
    Left err -> Left $ show err
    Right expr -> Right $ eval env expr