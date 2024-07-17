module Main (main) where

import End2End

import Evaluation

import Parser

import Test.Tasty (TestTree, defaultMain, testGroup)

import Units

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
        parserTests,
        evaluationTests,
        end2endTests,
        printUnitWithFractions
    ]
