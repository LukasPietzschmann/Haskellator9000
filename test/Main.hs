module Main (main) where

import Test.Tasty
import Math.SiConverter.Internal.Lexer (scan)
import Test.Tasty.HUnit ((@?=), testCase)
import Math.SiConverter.Internal.Parser (parse)
import Math.SiConverter.Internal.Expr
    ( Expr(..), Unit(..), Op(..) )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Simple expression parsing" [
    testCase "Simple integer multiplier" $ parse (scan "1") @?= Value 1 Multiplier,
    testCase "Simple floating multiplier" $ parse (scan "1.5") @?= Value 1.5 Multiplier,
    testCase "Simple addition" $ parse (scan "1 + 2") @?= BinOp (Value 1 Multiplier) Plus (Value 2 Multiplier),
    testCase "Precedence (* before +))" $ parse (scan "1 + 2 * 3") @?= BinOp (Value 1 Multiplier) Plus (BinOp (Value 2 Multiplier) Mult (Value 3 Multiplier)),
    testCase "Changed precedence (parentheses)" $ parse (scan "(1 + 2) * 3") @?= BinOp (BinOp (Value 1 Multiplier) Plus (Value 2 Multiplier)) Mult (Value 3 Multiplier)
  ]
