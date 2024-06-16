{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Monad (liftM3)
import Math.SiConverter.Internal.Expr ( Expr(..), Unit(..), Op(..) )
import Math.SiConverter.Internal.Lexer (scan)
import Math.SiConverter.Internal.Parser (parse, parseGracefully)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.QuickCheck (testProperty, Arbitrary, arbitrary, frequency, arbitraryBoundedEnum, suchThat)

instance Arbitrary Op where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Expr where
  arbitrary = let randomNumber = flip Value Multiplier <$> (arbitrary `suchThat` (>=0)) in frequency [
      (10, randomNumber),
      (6, liftM3 BinOp arbitrary arbitrary arbitrary),
      (3, UnaryOp Minus <$> randomNumber)
    ]

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Simple expression parsing" [
    testCase "Simple integer multiplier" $ parse (scan "1") @?= Value 1 Multiplier,
    testCase "Simple floating multiplier" $ parse (scan "1.5") @?= Value 1.5 Multiplier,
    testCase "Simple addition" $ parse (scan "1 + 2") @?= BinOp (Value 1 Multiplier) Plus (Value 2 Multiplier),
    testCase "Precedence (* before +)" $ parse (scan "1 + 2 * 3") @?= BinOp (Value 1 Multiplier) Plus (BinOp (Value 2 Multiplier) Mult (Value 3 Multiplier)),
    testCase "Precedence (^ before *)" $ parse (scan "2 ^ 3 * 4") @?= BinOp (BinOp (Value 2 Multiplier) Pow (Value 3 Multiplier)) Mult (Value 4 Multiplier),
    testCase "Changed precedence (parentheses)" $ parse (scan "(1 + 2) * 3") @?= BinOp (BinOp (Value 1 Multiplier) Plus (Value 2 Multiplier)) Mult (Value 3 Multiplier)
  ]

propertyTests :: TestTree
propertyTests = testGroup "Propery tests" [
    testProperty "Parser should not fail on valid input" $ \x -> either (const False) (const True) (parseGracefully $ scan $ show (x :: Expr))
  ]
