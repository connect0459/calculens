module Main where

import CalcuLens
import System.IO
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (unless)

-- REPLの実装
repl :: Environment -> IO ()
repl env = do
  putStr "calculens> "
  hFlush stdout
  line <- getLine
  unless (line == "quit" || line == "exit") $ do
    case parseAndEval env line of
      Left err -> do
        putStrLn $ "エラー: " ++ err
        repl env
      Right (val, newEnv) -> do
        putStrLn $ "結果: " ++ show val
        repl newEnv

main :: IO ()
main = do
  putStrLn "CalculensへようこそREPL! 数式を入力してください。終了するには 'quit' または 'exit' と入力してください。"
  -- 初期環境に定数を追加
  let initialEnv = Map.fromList
        [ ("pi", pi)
        , ("e", exp 1)
        ]
  repl initialEnv
