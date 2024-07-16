{-# LANGUAGE MultiWayIf #-}

-- | Tokenizes an input stream to a list of 'Token's
--
-- Arithmetic examples:
--
-- >>> scan "1+2"
-- Right [Number 1.0,Operator "+",Number 2.0]
--
-- >>> scan "(3/2+(1.5*2)) + 4.95"
-- Right [OpenParen,Number 3.0,Operator "/",Number 2.0,Operator "+",OpenParen,Number 1.5,Operator "*",Number 2.0,CloseParen,CloseParen,Operator "+",Number 4.95]
--
-- >>> scan "9001*29.12"
-- Right [Number 9001.0,Operator "*",Number 29.12]
--
-- Examples with units:
--
-- >>> scan "2km [m]"
-- Right [Number 2.0,Identifier "km",OpenBracket,Identifier "m",CloseBracket]
--
-- Examples with variables:
--
-- >>> scan "a = 3, b = 2 -> a + b"
-- Right [Identifier "a",Equal,Number 3.0,Comma,Identifier "b",Equal,Number 2.0,Arrow,Identifier "a",Operator "+",Identifier "b"]
--
module Math.Haskellator.Internal.Lexer (Token (..), Tokens, scan) where

import Data.Char

import Math.Haskellator.Internal.Utils.Error

data Token = Number Double -- ^ A number (integers are also represented as floats)
           | Operator String -- ^ An operator
           | OpenParen -- ^ Open parenthesis "("
           | CloseParen -- ^ Close parenthesis ")"
           | OpenBracket -- ^ Open bracket "["
           | CloseBracket -- ^ Close bracket "]"
           | Identifier String -- ^ Identifier (e.g. variable and function name) or unit
           | Arrow -- ^ Arrow "->"
           | Equal -- ^ Single equal sign "="
           | Comma -- ^ Comma ","
  deriving (Eq, Show)

-- | A simple alias for the 'Token' stream
type Tokens = [Token]

-- | Tokenizes an input stream to a list of 'Token's
scan :: String              -- ^ The input stream
     -> Either Error Tokens -- ^ Error message or the list of tokens
scan []           = Right []
scan ('(':xs)     = (OpenParen :)    <$> scan xs
scan (')':xs)     = (CloseParen :)   <$> scan xs
scan ('[':xs)     = (OpenBracket :)  <$> scan xs
scan (']':xs)     = (CloseBracket :) <$> scan xs
scan ('-':'>':xs) = (Arrow :)        <$> scan xs
scan ('+':xs)     = (Operator "+" :) <$> scan xs
scan ('-':xs)     = (Operator "-" :) <$> scan xs
scan ('*':xs)     = (Operator "*" :) <$> scan xs
scan ('/':xs)     = (Operator "/" :) <$> scan xs
scan ('^':xs)     = (Operator "^" :) <$> scan xs
scan ('=':xs)     = (Equal :)        <$> scan xs
scan (',':xs)     = (Comma :)        <$> scan xs
scan (x:xs)       = if | elem x [' ', '\t', '\r', '\n'] -> scan xs
                       | isDigit x -> scanNumber (x:xs)
                       | isAlpha x -> scanIdentifier (x:xs)
                       | otherwise -> Left $ Error ScanError $ "Unexpected character: " ++ [x]

scanNumber :: String -> Either Error Tokens
scanNumber xs = (Number (read num):) <$> scan rest
    where (num, rest) = span (\x -> any ($ x) [isDigit, (== '.')]) xs

scanIdentifier :: String -> Either Error Tokens
scanIdentifier xs = (Identifier i :) <$> scan rest
    where (i, rest) = span isAlpha xs
