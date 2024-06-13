{-# LANGUAGE MultiWayIf #-}

module Math.SiConverter.Lexer (Token(..), Tokens, scan) where

import Data.Char (isDigit)
import GHC.Unicode (isAlpha)

data Token = Number Double | Operator String | OpenParen | CloseParen | Identifier String
    deriving Show

type Tokens = [Token]

scan :: String -> Tokens
scan []       = []
scan ('(':xs) = OpenParen : scan xs
scan (')':xs) = CloseParen : scan xs
scan ('+':xs) = Operator "+" : scan xs
scan ('-':xs) = Operator "-" : scan xs
scan ('*':xs) = Operator "*" : scan xs
scan ('/':xs) = Operator "/" : scan xs
scan ('^':xs) = Operator "^" : scan xs
scan (x:xs)   = if | elem x [' ', '\t', '\r', '\n'] -> scan xs
                   | isDigit x -> scanNumber (x:xs)
                   | isAlpha x -> scanIdentifier (x:xs)
                   | otherwise -> error $ "Unexpected character: " ++ [x]

scanNumber :: String -> Tokens
scanNumber xs = Number (read num) : scan rest
    where (num, rest) = span (\x -> isDigit x || x == '.') xs

scanIdentifier :: String -> Tokens
scanIdentifier xs = Identifier i : scan rest
    where (i, rest) = span isAlpha xs
