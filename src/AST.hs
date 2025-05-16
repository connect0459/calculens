module AST
  ( Expr(..)
  , BinOp(..)
  , UnaryOp(..)
  ) where

data Expr
  = Num Double                -- 数値リテラル（例: 42.0）
  | Var String                -- 変数（例: x）
  | Binary BinOp Expr Expr    -- 二項演算（例: a + b）
  | Unary UnaryOp Expr        -- 単項演算（例: -a）
  | Call String [Expr]        -- 関数呼び出し（例: sin(x)）
  | Assign String Expr        -- 変数代入（例: x = 10）
  deriving (Show, Eq)

data BinOp = Add | Sub | Mul | Div | Pow
  deriving (Show, Eq)

data UnaryOp = Neg | Pos
  deriving (Show, Eq) 