{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | Parse a token stream to an expression tree
--
-- Examples:
--
-- >>> parse [Number 1.0,Operator "+",Number 2.0]
-- (1.0 + 2.0)
--
-- >>> parse [OpenParen,Number 3.0,Operator "/",Number 2.0,Operator "+",OpenParen,Number 1.5,Operator "*",Number 2.0,CloseParen,CloseParen,Operator "+",Number 4.95]
-- (((3.0 / 2.0) + (1.5 * 2.0)) + 4.95)
--
-- >>> parse [Number 9001.0,Operator "*",Number 29.12]
-- (9001.0 * 29.12)
--
-- We parse the following grammar:
--
-- > <expr> ::= <term> <expr'>
-- > <expr'> ::= "+" <term> <expr'>
-- >           | "-" <term> <expr'>
-- >           | ε
-- > <term> ::= <factor> <term'>
-- > <term'> ::= "*" <factor> <term'>
-- >           | "/" <factor> <term'>
-- >           | ε
-- > <factor> ::= "-" <primary>
-- >            | <primary>
-- > <primary> ::= <number>
-- >             | "(" <expr> ")"
-- > <number> ::= <value> <unit>
-- > <unit> ::= "m" | "s" | "kg" | ε
module Math.SiConverter.Internal.Parser (parseGracefully, parse) where

import Control.Applicative ((<|>))
import Control.Monad (liftM2)
import GHC.Base (Alternative (empty))
import Math.SiConverter.Internal.Expr
import Math.SiConverter.Internal.Lexer (Token(..), Tokens)

newtype Parser a = Parser { runParser :: Tokens -> Either String (a, Tokens) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
        (result, rest) <- p input
        return (f result, rest)

instance Applicative Parser where
    pure a = Parser $ Right . (a,)
    (Parser pf) <*> (Parser pp) = Parser $ \input -> do
        (f, res1) <- pf input
        (a, res2) <- pp res1
        return (f a, res2)

instance Monad Parser where
    (Parser p) >>= f = Parser $ \input -> do
        (a, res) <- p input
        runParser (f a) res

instance MonadFail Parser where
    fail err = Parser $ \_ -> Left err

instance Alternative Parser where
    empty = Parser $ \_ -> Left "Empty parser"
    (Parser p1) <|> (Parser p2) = Parser $ \input -> case p1 input of
        Left _ -> p2 input
        res    -> res

-- | Parse a token stream to an expression tree
parseGracefully :: Tokens             -- ^ Token stream
                -> Either String Expr -- ^ Error message or parsed expression
parseGracefully tokens = case runParser parseExpr tokens of
    Right (result, []) -> Right result
    Right (_, _)       -> Left "Parser was not abe to parse the full input"
    Left err           -> Left err

-- | Parse a token stream to an expression tree and throws if the input is invalid
parse :: Tokens -- ^ Token stream
      -> Expr   -- ^ Parsed expression
parse = either error id . parseGracefully

satisfy :: (Token -> Bool) -> Parser Token
satisfy predicate = Parser $ \case
    (x:xs) | predicate x -> Right (x, xs)
    _                    -> Left "Unexpected token"

isOperator :: Token -> Bool
isOperator (Operator _) = True
isOperator _            = False

isNumber :: Token -> Bool
isNumber (Number _) = True
isNumber _          = False

isIdentifier :: Token -> Bool
isIdentifier (Identifier _) = True
isIdentifier _              = False

requireToken :: Token -> Parser Token
requireToken t = satisfy (==t)

parseOperator :: Parser String
parseOperator = do
    Operator op <- satisfy isOperator
    return op

parseNumber :: Parser Double
parseNumber = do
  Number n <- satisfy isNumber
  return n

parseIdentifier :: Parser String
parseIdentifier = do
    Identifier i <- satisfy isIdentifier
    return i

parseUnit :: Parser Unit
parseUnit = do {
    u <- parseIdentifier;
    case u of
        "m"  -> return Meter
        "s"  -> return Second
        "kg" -> return Kilo
        x    -> fail $ "Invalid unit " ++ x
    } <|> return Multiplier

parseUnary :: Parser Op
parseUnary = do
    op <- parseOperator
    case op of
        "-" -> return UnaryMinus
        x   -> fail $ "Invalid unary operator " ++ x

parseTermOp :: Parser Op
parseTermOp = do
    op <- parseOperator
    case op of
      "+" -> return Plus
      "-" -> return Minus
      x   -> fail $ "Invalid binary operator " ++ x

parseFactorOp :: Parser Op
parseFactorOp = do
    op <- parseOperator
    case op of
      "^" -> return Pow
      "*" -> return Mult
      "/" -> return Div
      x   -> fail $ "Invalid binary operator " ++ x

parseExpr :: Parser Expr
parseExpr = parseTerm >>= expr'
    where expr' parsedLhs = do {
         liftM2 (BinOp parsedLhs) parseTermOp parseTerm >>= expr'
    } <|> return parsedLhs

parseTerm :: Parser Expr
parseTerm = parseFactor >>= term'
    where term' parsedLhs = do {
        liftM2 (BinOp parsedLhs) parseFactorOp parseFactor >>= term'
    } <|> return parsedLhs

parseFactor :: Parser Expr
parseFactor = liftM2 UnaryOp parseUnary parsePrimary <|> parsePrimary

parsePrimary :: Parser Expr
parsePrimary = parseValue <|> (requireToken OpenParen *> parseExpr <* requireToken CloseParen)

parseValue :: Parser Expr
parseValue = liftM2 Val parseNumber parseUnit
