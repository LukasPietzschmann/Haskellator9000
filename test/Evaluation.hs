module Evaluation (arithmetic) where
import Test.Tasty
import Math.SiConverter.Internal.Utils.Error
import Math.SiConverter.Internal.Lexer (scan)
import Math.SiConverter.Internal.Parser (parse)
import Math.SiConverter.Internal.AstProcessingSteps.Evaluate (evaluate)
import Control.Monad ((>=>))
import Test.Tasty.HUnit ((@?=), testCase)

evalString :: String -> Either Error Double
evalString = scan >=> parse >=> evaluate

arithmetic :: TestTree
arithmetic = testGroup "Simple arithmetic expressions" [
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
