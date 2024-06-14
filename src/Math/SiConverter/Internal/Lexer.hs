{-# LANGUAGE MultiWayIf #-}

module Math.SiConverter.Internal.Lexer (Token(..), Tokens, scan) where

import Data.Char (isDigit)
import GHC.Unicode (isAlpha)

data Token = Number Double | Operator String | OpenParen | CloseParen | Identifier String
    deriving (Show, Eq)

type Tokens = [Token]

-- | Tokenize an input string
--
-- Examples:
--
-- >>> scan "1+2"
-- [Number 1.0,Operator "+",Number 2.0]
--
-- >>> scan "(3/2+(1.5*2)) + 4.95"
-- [OpenParen,Number 3.0,Operator "/",Number 2.0,Operator "+",OpenParen,Number 1.5,Operator "*",Number 2.0,CloseParen,CloseParen,Operator "+",Number 4.95]
--
-- >>> scan "9001*29.12"
-- [Number 9001.0,Operator "*",Number 29.12]
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
