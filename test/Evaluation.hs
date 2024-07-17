module Evaluation (evaluationTests) where
import Control.Monad ((>=>))

import Math.Haskellator.Internal.AstProcessingSteps.Evaluate
import Math.Haskellator.Internal.AstProcessingSteps.Normalize
import Math.Haskellator.Internal.Expr
import Math.Haskellator.Internal.Lexer
import Math.Haskellator.Internal.Operators
import Math.Haskellator.Internal.Parser
import Math.Haskellator.Internal.Units
import Math.Haskellator.Internal.Utils.Error

import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

evaluationTests:: TestTree
evaluationTests = testGroup "Evaluation Tests" [arithmeticEval, normalization]

arithmeticEval :: TestTree
arithmeticEval = testGroup "Simple arithmetic expressions" [
    testCase "Constant" $ evalString "1" @?= Right 1,
    testCase "Addition" $ evalString "1 + 2" @?= Right 3,
    testCase "Subtraction" $ evalString "3 - 2" @?= Right 1,
    testCase "Multiplication" $ evalString "2 * 3" @?= Right 6,
    testCase "Division" $ evalString "6 / 3" @?= Right 2,
    testCase "Power" $ evalString "2 ^ 3" @?= Right 8,
    testCase "Unary minus" $ evalString "-2" @?= Right (-2),
    testCase "Unary minus in expression" $ evalString "1--2" @?= Right 3,
    testCase "Parenthesis" $ evalString "2(3+4)" @?= Right 14
  ]

normalization :: TestTree
normalization = testGroup "Normalization" [
    testCase "Kilometer -> Meter" $ normalizeString "1km" @?= Right (Val $ Value 1000 $ meter 1),
    testCase "Gram -> Kilogram" $ normalizeString "1000g" @?= Right (Val $ Value 1 $ kilogram 1),
    testCase "Day -> Seconds" $ normalizeString "1d" @?= Right (Val $ Value 86400 $ second 1),
    testCase "km/h -> m/s" $ normalizeString "3.6km/h"
      @?= Right (Val $ Value 1 $ meter 1 ++ second (-1)),
    testCase "Does not change exponents" $ normalizeString "1m^42 / 1s^33"
      @?= Right (BinOp (Val $ Value 1 $ meter 42) Div (Val $ Value 1 $ second 33))
  ]

evalString :: String -> Either Error Double
evalString = scan >=> parse >=> evaluate

normalizeString :: String -> Either Error Expr
normalizeString = scan >=> parse >=> normalize
