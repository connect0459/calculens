module Parser
  ( parseExpr
  ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (emptyDef)
import Control.Monad.Identity (Identity)

import AST

-- 字句解析器の定義
lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    style = emptyDef
      { Token.commentLine     = "//"
      , Token.commentStart    = "/*"
      , Token.commentEnd      = "*/"
      , Token.identStart      = letter <|> char '_'
      , Token.identLetter     = alphaNum <|> char '_'
      , Token.reservedNames   = ["let", "in", "if", "then", "else"]
      , Token.reservedOpNames = ["+", "-", "*", "/", "^", "="]
      }

-- パーサーのヘルパー関数
integer :: Parser Integer
integer = Token.integer lexer

float :: Parser Double
float = try (Token.float lexer) <|> (fromIntegral <$> integer)

parens :: Parser a -> Parser a
parens = Token.parens lexer

identifier :: Parser String
identifier = Token.identifier lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

-- 式のパーサー
expr :: Parser Expr
expr = buildExpressionParser operators term

-- 演算子テーブル（優先順位順）
operators :: [[Operator String () Identity Expr]]
operators =
  [ [ Prefix (reservedOp "-" >> return (Unary Neg))
    , Prefix (reservedOp "+" >> return (Unary Pos))
    ]
  , [ Infix (reservedOp "^" >> return (Binary Pow)) AssocRight ]
  , [ Infix (reservedOp "*" >> return (Binary Mul)) AssocLeft
    , Infix (reservedOp "/" >> return (Binary Div)) AssocLeft
    ]
  , [ Infix (reservedOp "+" >> return (Binary Add)) AssocLeft
    , Infix (reservedOp "-" >> return (Binary Sub)) AssocLeft
    ]
  , [ Infix assignOp AssocRight ]
  ]

assignOp :: ParsecT String () Identity (Expr -> Expr -> Expr)
assignOp = do
  reservedOp "="
  return $ \x y -> case x of
    Var name -> Assign name y
    _ -> error "左辺は変数である必要があります"

-- 項のパーサー
term :: Parser Expr
term =  parens expr
    <|> try functionCall
    <|> try (Num <$> float)
    <|> (Var <$> identifier)

-- 関数呼び出しのパーサー
functionCall :: Parser Expr
functionCall = do
  name <- identifier
  args <- parens $ expr `sepBy` (Token.comma lexer)
  return $ Call name args

-- メイン式パーサー
parseExpr :: String -> Either ParseError Expr
parseExpr input = parse (expr <* eof) "" input 