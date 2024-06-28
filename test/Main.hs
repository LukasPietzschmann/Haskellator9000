{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Monad (liftM3)

import Math.SiConverter.Internal.Expr (Expr (..), Op (..), Unit (..), Value (..))
import Math.SiConverter.Internal.Lexer (scan)
import Math.SiConverter.Internal.Parser (parse)
import Math.SiConverter.Internal.Utils.Composition ((.:))

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
  arbitrary = let randomNumber = flip (Val .: Value) Multiplier <$> genNumber in frequency [
      (10, randomNumber),
      (6, liftM3 BinOp arbitrary arbitrary arbitrary),
      (1, UnaryOp Minus <$> randomNumber)
    ]

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Simple expression parsing" [
    testCase "Simple integer multiplier" $ (scan "1" >>= parse) @?= Right (Val (Value 1 Multiplier)),
    testCase "Simple floating multiplier" $ (scan "1.5" >>= parse) @?= Right (Val (Value 1.5 Multiplier)),
    testCase "Simple addition" $ (scan "1 + 2" >>= parse) @?= Right (BinOp (Val $ Value 1 Multiplier) Plus (Val $ Value 2 Multiplier)),
    testCase "Precedence (* before +)" $ (scan "1 + 2 * 3" >>= parse) @?= Right (BinOp (Val $ Value 1 Multiplier) Plus (BinOp (Val $ Value 2 Multiplier) Mult (Val $ Value 3 Multiplier))),
    testCase "Precedence (^ before *)" $ (scan "2 ^ 3 * 4" >>= parse) @?= Right (BinOp (BinOp (Val $ Value 2 Multiplier) Pow (Val $ Value 3 Multiplier)) Mult (Val $ Value 4 Multiplier)),
    testCase "Changed precedence (parentheses)" $ (scan "(1 + 2) * 3" >>= parse) @?= Right (BinOp (BinOp (Val $ Value 1 Multiplier) Plus (Val $ Value 2 Multiplier)) Mult (Val $ Value 3 Multiplier))
  ]

propertyTests :: TestTree
propertyTests = testGroup "Propery tests" [
    testProperty "Parser should not fail on valid input" $ \x -> either (const False) (const True) (scan (show (x :: Expr)) >>= parse)
  ]
