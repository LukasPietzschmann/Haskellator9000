{-# OPTIONS_GHC -Wno-orphans #-}

module Parser (parserTests) where

import Control.Monad

import Data.Either

import Math.Haskellator.Internal.Expr
import Math.Haskellator.Internal.Lexer
import Math.Haskellator.Internal.Operators
import Math.Haskellator.Internal.Parser
import Math.Haskellator.Internal.Units
import Math.Haskellator.Internal.Utils.Error

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

parserTests :: TestTree
parserTests = testGroup "ParserTests" [expressionParsing, parserProperties]

expressionParsing :: TestTree
expressionParsing = testGroup "Simple expression parsing" [
    testCase "Integer multiplier"
      $ parseString "1" @?= Right (Val (Value 1 $ multiplier 1)),
    testCase "Floating-point multiplier"
      $ parseString "1.5" @?= Right (Val (Value 1.5 $ multiplier 1)),
    testCase "Addition"
      $ parseString "1 + 2" @?= Right (BinOp (Val $ Value 1 $ multiplier 1) Plus (Val $ Value 2 $ multiplier 1)),
    testCase "Precedence (* before +)"
      $ parseString "1 + 2 * 3" @?= Right (BinOp (Val $ Value 1 $ multiplier 1) Plus (BinOp (Val $ Value 2 $ multiplier 1) Mult (Val $ Value 3 $ multiplier 1))),
    testCase "Precedence (^ before *)"
      $ parseString "2 ^ 3 * 4" @?= Right (BinOp (BinOp (Val $ Value 2 $ multiplier 1) Pow (Val $ Value 3 $ multiplier 1)) Mult (Val $ Value 4 $ multiplier 1)),
    testCase "Changed precedence (parentheses)"
      $ parseString "(1 + 2) * 3" @?= Right (BinOp (BinOp (Val $ Value 1 $ multiplier 1) Plus (Val $ Value 2 $ multiplier 1)) Mult (Val $ Value 3 $ multiplier 1)),
    testCase "Minus and unary minus"
      $ parseString "1--2" @?= Right (BinOp (Val $ Value 1 $ multiplier 1) Minus (UnaryOp Minus (Val $ Value 2 $ multiplier 1))),
    testCase "Multiplication operand can be omitted"
      $ parseString "2(3+1)" @?= Right (BinOp (Val $ Value 2 $ multiplier 1) Mult (BinOp (Val $ Value 3 $ multiplier 1) Plus (Val $ Value 1 $ multiplier 1)))
  ]

parseString :: String -> Either Error Expr
parseString = scan >=> parse

parserProperties :: TestTree
parserProperties = testGroup "Parser properties" [
    testProperty "Parser should not fail on valid input" $ \x -> either (const False) (const True) (parseString (show (x :: Expr))),
    testProperty "Idempotence of the parser" $ \x -> fromRight False (scan (show (x :: Expr)) >>= parse >>= Right . (==x))
  ]

instance Arbitrary Op where
    arbitrary = arbitraryBoundedEnum

genNumber :: Gen Double
genNumber = do
    n <- choose (0, 999999)
    return $ fromInteger n / 1000.0

genInt :: Gen Int
genInt = do
    n <- choose (0, 99)
    return $ fromInteger n

instance Arbitrary Unit where
    -- Multiplier needs to be excluded here to prevent ambiguous cases in our grammar
    -- e.g. 2^2 could be either a multiplier with exponent two or a power operation on two multipliers with exponent 1
    arbitrary = arbitraryBoundedEnum `suchThat` (not . isMultiplier)

instance Arbitrary Expr where
  arbitrary = let randomValue = do {
     val <- genNumber;
     ex <- genInt;
     unit <- arbitrary;
     return $ Val $ Value val [UnitExp unit ex]
   }
    in frequency [
      (10, randomValue),
      (6, liftM3 BinOp arbitrary arbitrary arbitrary),
      (1, UnaryOp Minus <$> randomValue)
    ]
