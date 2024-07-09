module Main (main) where

import Evaluation ( simpleEvalTests )
import Parser ( simpleExprParseTests, properties )
import Test.Tasty (TestTree, testGroup, defaultMain)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [simpleExprParseTests, properties, simpleEvalTests ]