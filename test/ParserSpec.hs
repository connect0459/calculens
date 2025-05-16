module ParserSpec (spec) where

import Test.Hspec
import AST
import Parser

spec :: Spec
spec = do
  describe "Expression Parser" $ do
    it "should parse numbers" $ do
      parseExpr "42" `shouldBe` Right (Num 42.0)
      parseExpr "3.14" `shouldBe` Right (Num 3.14)

    it "should parse variables" $ do
      parseExpr "x" `shouldBe` Right (Var "x")
      parseExpr "foo" `shouldBe` Right (Var "foo")

    it "should parse basic arithmetic" $ do
      parseExpr "1 + 2" `shouldBe` Right (Binary Add (Num 1.0) (Num 2.0))
      parseExpr "3 - 4" `shouldBe` Right (Binary Sub (Num 3.0) (Num 4.0))
      parseExpr "5 * 6" `shouldBe` Right (Binary Mul (Num 5.0) (Num 6.0))
      parseExpr "8 / 2" `shouldBe` Right (Binary Div (Num 8.0) (Num 2.0))

    it "should respect operator precedence" $ do
      parseExpr "1 + 2 * 3" `shouldBe` 
        Right (Binary Add (Num 1.0) (Binary Mul (Num 2.0) (Num 3.0)))
      parseExpr "1 * 2 + 3" `shouldBe`
        Right (Binary Add (Binary Mul (Num 1.0) (Num 2.0)) (Num 3.0))

    it "should parse parentheses" $ do
      parseExpr "(1 + 2) * 3" `shouldBe`
        Right (Binary Mul (Binary Add (Num 1.0) (Num 2.0)) (Num 3.0))

    it "should parse unary operators" $ do
      parseExpr "-42" `shouldBe` Right (Unary Neg (Num 42.0))
      parseExpr "+42" `shouldBe` Right (Unary Pos (Num 42.0))

    it "should parse function calls" $ do
      parseExpr "sin(0)" `shouldBe` Right (Call "sin" [Num 0.0])
      parseExpr "max(1, 2)" `shouldBe` Right (Call "max" [Num 1.0, Num 2.0])

    it "should parse assignments" $ do
      parseExpr "x = 42" `shouldBe` Right (Assign "x" (Num 42.0))
      parseExpr "y = x + 1" `shouldBe` 
        Right (Assign "y" (Binary Add (Var "x") (Num 1.0)))

    it "should handle whitespace" $ do
      parseExpr " 1  +  2 " `shouldBe` Right (Binary Add (Num 1.0) (Num 2.0))

    it "should fail on invalid input" $ do
      case parseExpr "1 +" of
        Left _ -> return ()
        Right expr -> expectationFailure $ 
          "Expected parse error, but got: " ++ show expr 