module ASTSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import AST

spec :: Spec
spec = do
  describe "AST Data Types" $ do
    it "should create a number expression" $ do
      let expr = Num 42.0
      case expr of
        Num n -> n `shouldBe` 42.0
        _ -> expectationFailure "Expected Num constructor"

    it "should create a variable expression" $ do
      let expr = Var "x"
      case expr of
        Var name -> name `shouldBe` "x"
        _ -> expectationFailure "Expected Var constructor"

    it "should create a binary operation" $ do
      let expr = Binary Add (Num 1.0) (Num 2.0)
      case expr of
        Binary op left right -> do
          op `shouldBe` Add
          left `shouldBe` Num 1.0
          right `shouldBe` Num 2.0
        _ -> expectationFailure "Expected Binary constructor"

    it "should create a unary operation" $ do
      let expr = Unary Neg (Num 1.0)
      case expr of
        Unary op operand -> do
          op `shouldBe` Neg
          operand `shouldBe` Num 1.0
        _ -> expectationFailure "Expected Unary constructor"

    it "should create a function call" $ do
      let expr = Call "sin" [Num 0.0]
      case expr of
        Call name args -> do
          name `shouldBe` "sin"
          args `shouldBe` [Num 0.0]
        _ -> expectationFailure "Expected Call constructor"

    it "should create an assignment" $ do
      let expr = Assign "x" (Num 42.0)
      case expr of
        Assign name value -> do
          name `shouldBe` "x"
          value `shouldBe` Num 42.0
        _ -> expectationFailure "Expected Assign constructor"

  -- QuickCheck property tests
  describe "AST Properties" $ do
    it "should maintain equality for same expressions" $ property $
      \n -> Num n == Num n

    it "should maintain inequality for different expressions" $ property $
      \n m -> n /= m ==> Num n /= Num m 