{-# LANGUAGE LambdaCase #-}
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
import Control.Monad.State
import Data.Bifunctor (first)
import GHC.Base (Alternative (empty))
import Math.SiConverter.Internal.Expr
import Math.SiConverter.Internal.Lexer (Token(..), Tokens)
import Math.SiConverter.Internal.Utils.Composition ((.:))

newtype ParserT m a = ParserT { runParserT :: Tokens -> m (Either String (a, Tokens)) }

-- | Parser monad. The state encodes weather or not we're coming from a factor. This is
-- only used when parsing things like "2m*s" where there's no explicit value for the seconds.
-- And since "2m + s" is invalid, we can use this to differentiate between the two.
type Parser = ParserT (State Bool)

runParser :: Parser a -> Tokens -> Either String (a, Tokens)
runParser p ts = evalState (runParserT p ts) False


instance Functor m => Functor (ParserT m) where
    fmap f (ParserT p) = ParserT $ \input -> fmap (first f) <$> p input

instance Monad m => Applicative (ParserT m) where
    pure a = ParserT $ \input -> return $ Right (a, input)
    (ParserT lhs) <*> (ParserT rhs) = ParserT $ \input -> do
        lhs input >>= \case
            Left err -> return $ Left err
            Right (f, ts) -> rhs ts >>= \case
                Left err -> return $ Left err
                Right (res, ts') -> return $ Right (f res, ts')

instance Monad m => Monad (ParserT m) where
    (ParserT p) >>= f = ParserT $ \input -> do
        p input >>= \case
            Left err        -> return $ Left err
            Right (res, ts) -> runParserT (f res) ts

instance Monad m => MonadFail (ParserT m) where
    fail = ParserT . const . return . Left

instance Monad m => Alternative (ParserT m) where
    empty = ParserT $ const $ return $ Left "Empty parser"
    (ParserT p1) <|> (ParserT p2) = ParserT $ \input -> do
        result <- p1 input
        case result of
            Left _  -> p2 input
            Right _ -> return result

instance MonadTrans ParserT where
    lift op = ParserT $ \input -> do
        result <- op
        return $ Right (result, input)

instance MonadIO m => MonadIO (ParserT m) where
    liftIO = lift . liftIO

-- | Parse a token stream to an expression tree
parseGracefully :: Tokens             -- ^ Token stream
                -> Either String Expr -- ^ Error message or parsed expression
parseGracefully tokens = case runParser parseExpr tokens of
    Right (result, []) -> Right result
    Right (_, ts)      -> Left $ "Parser was unable to parse the full input. " ++ show ts ++ " remains in the token stream."
    Left err           -> Left err

-- | Parse a token stream to an expression tree and throws if the input is invalid
parse :: Tokens -- ^ Token stream
      -> Either String Expr   -- ^ Parsed expression
parse = parseGracefully

satisfy :: (Token -> Bool) -> Parser Token
satisfy predicate = ParserT $ \input -> return $ case input of
    (x:xs) | predicate x -> Right (x, xs)
           | otherwise   -> Left $ "Unexpected token " ++ show x
    _                    -> Left "Reached unexpected end of token stream"

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
    either (\x -> fail $ "Invalid unit " ++ x) return (unitFromString u)
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
    lift $ put False
    case op of
      "+" -> return Plus
      "-" -> return Minus
      x   -> fail $ "Invalid binary operator " ++ x

parseFactorOp :: Parser Op
parseFactorOp = do
    op <- parseOperator
    lift $ put True
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
parsePrimary = (requireToken OpenParen *> parseExpr <* requireToken CloseParen) <|> parseValue

parseValue :: Parser Expr
parseValue = liftM2 (Val .: Value) parseNumber parseUnit <|> do
    isInFactor <- lift get
    if isInFactor
        then Val . Value 1 <$> parseUnit
        else fail "Value-less units are only allowed in factors"
