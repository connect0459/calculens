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
import Control.Exception (evaluate, try, ErrorCall(..))
import System.IO.Unsafe (unsafePerformIO)

-- パースして評価する便利関数
parseAndEval :: Environment -> String -> Either String (Double, Environment)
parseAndEval env input =
  case parseExpr input of
    Left err -> Left $ show err
    Right expr -> 
      case unsafePerformIO $ try (evaluate $ eval env expr) of
        Left (ErrorCall e) -> Left e
        Right result -> Right result