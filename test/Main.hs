{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Main (main) where

import Control.Monad (liftM3, (>=>))

import Data.Either (fromRight)

import Math.SiConverter.Internal.Evaluate (evaluate)
import Math.SiConverter.Internal.Expr (Expr (..), Op (..), Unit (..), Value (..))
import Math.SiConverter.Internal.Lexer (scan)
import Math.SiConverter.Internal.Parser (parse)
import Math.SiConverter.Internal.Utils.Composition ((.:))
import Math.SiConverter.Internal.Utils.Error (Error)

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (Arbitrary, Gen, arbitrary, arbitraryBoundedEnum, choose,
           frequency, testProperty)

instance Arbitrary Op where
    arbitrary = arbitraryBoundedEnum

genNumber :: Gen Double
genNumber = do
    n <- choose (0, 999999)
    return $ fromInteger n / 1000.0

instance Arbitrary Expr where
  arbitrary = let randomNumber = flip (Val .: Value) (Multiplier 1) <$> genNumber in frequency [
      (10, randomNumber),
      (6, liftM3 BinOp arbitrary arbitrary arbitrary),
      (1, UnaryOp Minus <$> randomNumber)
    ]

parseString :: String -> Either Error Expr
parseString = scan >=> parse

evalString :: String -> Either Error Double
evalString = scan >=> parse >=> evaluate

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [simpleExprParseTests, simpleEvalTests, propertyTests]

simpleExprParseTests :: TestTree
simpleExprParseTests = testGroup "Simple expression parsing" [
    testCase "Simple integer multiplier" $ parseString "1" @?= Right (Val (Value 1 $ Multiplier 1)),
    testCase "Simple floating multiplier" $ parseString "1.5" @?= Right (Val (Value 1.5 $ Multiplier 1)),
    testCase "Simple addition" $ parseString "1 + 2" @?= Right (BinOp (Val $ Value 1 $ Multiplier 1) Plus (Val $ Value 2 $ Multiplier 1)),
    testCase "Precedence (* before +)" $ parseString "1 + 2 * 3" @?= Right (BinOp (Val $ Value 1 $ Multiplier 1) Plus (BinOp (Val $ Value 2 $ Multiplier 1) Mult (Val $ Value 3 $ Multiplier 1))),
    testCase "Precedence (^ before *)" $ parseString "2 ^ 3 * 4" @?= Right (BinOp (BinOp (Val $ Value 2 $ Multiplier 1) Pow (Val $ Value 3 $ Multiplier 1)) Mult (Val $ Value 4 $ Multiplier 1)),
    testCase "Changed precedence (parentheses)" $ parseString "(1 + 2) * 3" @?= Right (BinOp (BinOp (Val $ Value 1 $ Multiplier 1) Plus (Val $ Value 2 $ Multiplier 1)) Mult (Val $ Value 3 $ Multiplier 1)),
    testCase "Minus and unary minus" $ parseString "1--2" @?= Right (BinOp (Val $ Value 1 $ Multiplier 1) Minus (UnaryOp Minus (Val $ Value 2 $ Multiplier 1))),
    testCase "Mult oper can be omitted" $ parseString "2(3+1)" @?= Right (BinOp (Val $ Value 2 $ Multiplier 1) Mult (BinOp (Val $ Value 3 $ Multiplier 1) Plus (Val $ Value 1 $ Multiplier 1)))
  ]

simpleEvalTests :: TestTree
simpleEvalTests = testGroup "Simple expression evaluation" [
    testCase "Constant" $ evalString "1" @?= Right 1,
    testCase "Addition" $ evalString "1 + 2" @?= Right 3,
    testCase "Subtraction" $ evalString "3 - 2" @?= Right 1,
    testCase "Multiplication" $ evalString "2 * 3" @?= Right 6,
    testCase "Division" $ evalString "6 / 3" @?= Right 2,
    testCase "Power" $ evalString "2 ^ 3" @?= Right 8,
    testCase "Unary minus" $ evalString "-2" @?= Right (-2),
    testCase "Unary minus in expression" $ evalString "1--2" @?= Right 3
  ]

propertyTests :: TestTree
propertyTests = testGroup "Propery tests" [
    testProperty "Parser should not fail on valid input" $ \x -> either (const False) (const True) (parseString (show (x :: Expr))),
    testProperty "Idempotence of the parser" $ \x -> fromRight False (scan (show (x :: Expr)) >>= parse >>= Right . (==x))
  ]
