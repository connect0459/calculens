module EvaluatorSpec (spec) where

import Test.Hspec
import AST
import Evaluator
import qualified Data.Map as Map
import Control.Exception (evaluate)

spec :: Spec
spec = do
  describe "Expression Evaluator" $ do
    let emptyEnv = Map.empty
    let testEnv = Map.fromList [("x", 10.0), ("y", 20.0)]

    it "should evaluate numbers" $ do
      eval emptyEnv (Num 42.0) `shouldBe` (42.0, emptyEnv)

    it "should evaluate variables" $ do
      eval testEnv (Var "x") `shouldBe` (10.0, testEnv)
      eval testEnv (Var "y") `shouldBe` (20.0, testEnv)

    it "should handle undefined variables" $ do
      evaluate (fst $ eval emptyEnv (Var "undefined")) `shouldThrow` anyException

    it "should evaluate binary operations" $ do
      eval emptyEnv (Binary Add (Num 1.0) (Num 2.0)) `shouldBe` (3.0, emptyEnv)
      eval emptyEnv (Binary Sub (Num 5.0) (Num 3.0)) `shouldBe` (2.0, emptyEnv)
      eval emptyEnv (Binary Mul (Num 4.0) (Num 2.0)) `shouldBe` (8.0, emptyEnv)
      eval emptyEnv (Binary Div (Num 6.0) (Num 2.0)) `shouldBe` (3.0, emptyEnv)
      eval emptyEnv (Binary Pow (Num 2.0) (Num 3.0)) `shouldBe` (8.0, emptyEnv)

    it "should evaluate unary operations" $ do
      eval emptyEnv (Unary Neg (Num 42.0)) `shouldBe` (-42.0, emptyEnv)
      eval emptyEnv (Unary Pos (Num 42.0)) `shouldBe` (42.0, emptyEnv)

    it "should evaluate function calls" $ do
      let pi_2 = pi / 2
      eval emptyEnv (Call "sin" [Num pi_2]) `shouldBe` (1.0, emptyEnv)
      eval emptyEnv (Call "cos" [Num 0.0]) `shouldBe` (1.0, emptyEnv)
      eval emptyEnv (Call "sqrt" [Num 4.0]) `shouldBe` (2.0, emptyEnv)

    it "should handle invalid function calls" $ do
      evaluate (fst $ eval emptyEnv (Call "undefined" [Num 0.0])) `shouldThrow` anyException
      evaluate (fst $ eval emptyEnv (Call "sin" [])) `shouldThrow` anyException

    it "should evaluate assignments" $ do
      let (val, newEnv) = eval emptyEnv (Assign "x" (Num 42.0))
      val `shouldBe` 42.0
      Map.lookup "x" newEnv `shouldBe` Just 42.0

    it "should handle complex expressions" $ do
      let expr = Binary Add 
                  (Binary Mul (Var "x") (Num 2.0))
                  (Binary Div (Var "y") (Num 4.0))
      let (result, _) = eval testEnv expr
      result `shouldBe` 25.0  -- (10 * 2) + (20 / 4) 