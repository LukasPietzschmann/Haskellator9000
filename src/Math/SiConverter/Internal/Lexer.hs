{-# LANGUAGE MultiWayIf #-}

-- | Tokenizes an input stream to a list of 'Token's
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
module Math.SiConverter.Internal.Lexer (Token(..), Tokens, scan) where

import Data.Char (isDigit)
import GHC.Unicode (isAlpha)

data Token = Number Double -- ^ A number (integers are also represented as floats)
    | Operator String      -- ^ An operator
    | OpenParen            -- ^ Open parenthesis "("
    | CloseParen           -- ^ Close parenthesis ")"
    | Identifier String    -- ^ Identifier (e.g. variable and function name) or unit
    deriving (Show, Eq)

-- | A simple alias for the 'Token' stream
type Tokens = [Token]

-- | Tokenizes an input stream to a list of 'Token's
scan :: String               -- ^ The input stream
               -> Either String Tokens -- ^ Error message or the list of tokens
scan []       = Right []
scan ('(':xs) = (OpenParen :)    <$> scan xs
scan (')':xs) = (CloseParen :)   <$> scan xs
scan ('+':xs) = (Operator "+" :) <$> scan xs
scan ('-':xs) = (Operator "-" :) <$> scan xs
scan ('*':xs) = (Operator "*" :) <$> scan xs
scan ('/':xs) = (Operator "/" :) <$> scan xs
scan ('^':xs) = (Operator "^" :) <$> scan xs
scan (x:xs)   = if | elem x [' ', '\t', '\r', '\n'] -> scan xs
                             | isDigit x -> scanNumber (x:xs)
                             | isAlpha x -> scanIdentifier (x:xs)
                             | otherwise -> Left $ "Unexpected character: " ++ [x]

scanNumber :: String -> Either String Tokens
scanNumber xs = (Number (read num):) <$> scan rest
    where (num, rest) = span (\x -> any ($ x) [isDigit, (== '.'), (== 'e'), (== '-')]) xs

scanIdentifier :: String -> Either String Tokens
scanIdentifier xs = (Identifier i :) <$> scan rest
    where (i, rest) = span isAlpha xs
