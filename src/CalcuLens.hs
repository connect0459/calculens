module CalcuLens
  ( parseAndEval
  , parseExpr
  , eval
  , Expr(..)
  , Environment
  ) where

import AST
import Parser
import Evaluator
import qualified Data.Map as Map

-- パースして評価する便利関数
parseAndEval :: Environment -> String -> Either String (Double, Environment)
parseAndEval env input = 
  case parseExpr input of
    Left err -> Left $ show err
    Right expr -> Right $ eval env expr 