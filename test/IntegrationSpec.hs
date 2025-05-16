module IntegrationSpec (spec) where

import Test.Hspec
import CalcuLens
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "Calculator Integration" $ do
    let emptyEnv = Map.empty
    let initialEnv = Map.fromList [("pi", pi), ("e", exp 1)]

    it "should evaluate simple arithmetic expressions" $ do
      parseAndEval emptyEnv "2 + 3 * 4" `shouldBe` Right (14.0, emptyEnv)
      parseAndEval emptyEnv "(2 + 3) * 4" `shouldBe` Right (20.0, emptyEnv)

    it "should handle variables and assignments" $ do
      case parseAndEval emptyEnv "x = 5" of
        Right (val1, env1) -> do
          val1 `shouldBe` 5.0
          case parseAndEval env1 "y = x + 3" of
            Right (val2, env2) -> do
              val2 `shouldBe` 8.0
              case parseAndEval env2 "x * y" of
                Right (val3, _) -> val3 `shouldBe` 40.0
                Left err -> expectationFailure $ "Unexpected error: " ++ err
            Left err -> expectationFailure $ "Unexpected error: " ++ err
        Left err -> expectationFailure $ "Unexpected error: " ++ err

    it "should evaluate mathematical functions" $ do
      case parseAndEval initialEnv "sin(pi/2)" of
        Right (val1, _) -> val1 `shouldBe` 1.0
        Left err -> expectationFailure $ "Unexpected error: " ++ err

      case parseAndEval initialEnv "cos(0)" of
        Right (val2, _) -> val2 `shouldBe` 1.0
        Left err -> expectationFailure $ "Unexpected error: " ++ err

      case parseAndEval initialEnv "sqrt(16)" of
        Right (val3, _) -> val3 `shouldBe` 4.0
        Left err -> expectationFailure $ "Unexpected error: " ++ err

    it "should handle complex expressions" $ do
      let expr = "x = 2 * pi"
      case parseAndEval initialEnv expr of
        Right (_, env1) -> 
          case parseAndEval env1 "sin(x/4)^2 + cos(x/4)^2" of
            Right (val, _) -> val `shouldBe` 1.0  -- sin²(θ) + cos²(θ) = 1
            Left err -> expectationFailure $ "Unexpected error: " ++ err
        Left err -> expectationFailure $ "Unexpected error: " ++ err

    it "should handle error cases" $ do
      parseAndEval emptyEnv "1 +" `shouldBe` Left "(line 1, column 4):\nunexpected end of input\nexpecting end of \"+\", \"(\", identifier, float or integer"
      parseAndEval emptyEnv "1 / 0" `shouldBe` Right (1/0, emptyEnv)  -- IEEE 754 infinity
      case parseAndEval emptyEnv "undefined_var" of
        Left _ -> return ()
        Right _ -> expectationFailure "Expected error for undefined variable" 