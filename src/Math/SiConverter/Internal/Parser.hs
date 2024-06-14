{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Math.SiConverter.Internal.Parser (parseGracefully, parse) where

import Control.Applicative ((<|>))
import GHC.Base (Alternative (empty))
import Math.SiConverter.Internal.Expr
import Math.SiConverter.Internal.Lexer (Token(..), Tokens)

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
-- Grammar:
--
-- <expr> ::= <term> <expr'>
-- <expr'> ::= "+" <term> <expr'>
          -- | "-" <term> <expr'>
          -- | ε
-- <term> ::= <factor> <term'>
-- <term'> ::= "*" <factor> <term'>
          -- | "/" <factor> <term'>
          -- | ε
-- <factor> ::= "-" <primary>
           -- | <primary>
-- <primary> ::= <number>
            -- | "(" <expr> ")"

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

parseGracefully :: Tokens -> Either String Expr
parseGracefully tokens = case runParser parseExpr tokens of
  Right (result, []) -> Right result
  Right (_, _) -> Left "Parser was not abe to parse the full input"
  Left err -> Left err

parse :: Tokens -> Expr
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

token :: Token -> Parser Token
token t = satisfy (==t)

operator :: Parser String
operator = do
    Operator op <- satisfy isOperator
    return op

number :: Parser Double
number = do
  Number n <- satisfy isNumber
  return n

unary :: Parser Op
unary = do
    op <- operator
    case op of
        "-" -> return UnaryMinus
        x   -> fail $ "Invalid unary operator " ++ x

term :: Parser Op
term = do
    op <- operator
    case op of
      "+" -> return Plus
      "-" -> return Minus
      x   -> fail $ "Invalid binary operator " ++ x

factor :: Parser Op
factor = do
    op <- operator
    case op of
      "*" -> return Mult
      "/" -> return Div
      x   -> fail $ "Invalid binary operator " ++ x

parseExpr :: Parser Expr
parseExpr = parseTerm >>= expr'
    where expr' parsedLhs = do {
        op <- term;
        right <- parseTerm;
        expr' (BinOp parsedLhs op right)
    } <|> return parsedLhs

parseTerm :: Parser Expr
parseTerm = parseFactor >>= term'
    where term' parsedLhs = do {
        op <- factor;
        right <- parseFactor;
        term' (BinOp parsedLhs op right)
    } <|> return parsedLhs

parseFactor :: Parser Expr
parseFactor = do {
    op <- unary;
    expr <- parsePrimary;
    return (UnaryOp op expr)
  } <|> parsePrimary

parsePrimary :: Parser Expr
parsePrimary = (flip Value Multiplier <$> number) <|> (token OpenParen *> parseExpr <* token CloseParen)
