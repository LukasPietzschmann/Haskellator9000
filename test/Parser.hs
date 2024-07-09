{-# OPTIONS_GHC -Wno-orphans #-}

module Parser (expressionParsing, parserProperties) where
import Test.Tasty
import Math.SiConverter.Internal.Expr
    ( Value(Value),
      Expr(UnaryOp, BinOp, Val),
      multiplier,
      Op(Plus, Pow, Minus, Mult) )
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary (arbitrary), Gen, choose, arbitraryBoundedEnum, frequency)
import Data.Either (fromRight)
import Math.SiConverter.Internal.Lexer (scan)
import Math.SiConverter.Internal.Parser (parse)
import Math.SiConverter.Internal.Utils.Error (Error)
import Control.Monad ((>=>), liftM3)
import Math.SiConverter.Internal.Utils.Composition ((.:))

parseString :: String -> Either Error Expr
parseString = scan >=> parse

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

instance Arbitrary Op where
    arbitrary = arbitraryBoundedEnum

genNumber :: Gen Double
genNumber = do
    n <- choose (0, 999999)
    return $ fromInteger n / 1000.0

instance Arbitrary Expr where
  arbitrary = let randomNumber = flip (Val .: Value) (multiplier 1) <$> genNumber in frequency [
      (10, randomNumber),
      (6, liftM3 BinOp arbitrary arbitrary arbitrary),
      (1, UnaryOp Minus <$> randomNumber)
    ]

parserProperties :: TestTree
parserProperties = testGroup "Parser properties" [
    testProperty "Parser should not fail on valid input" $ \x -> either (const False) (const True) (parseString (show (x :: Expr))),
    testProperty "Idempotence of the parser" $ \x -> fromRight False (scan (show (x :: Expr)) >>= parse >>= Right . (==x))
  ]