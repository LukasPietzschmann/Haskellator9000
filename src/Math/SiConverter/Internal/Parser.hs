{-# LANGUAGE LambdaCase #-}
-- | Parse a token stream to an expression tree
--
-- Examples:
--
-- >>> parse [Number 1.0,Operator "+",Number 2.0]
-- Right (1.0 + 2.0)
--
-- >>> parse [OpenParen,Number 3.0,Operator "/",Number 2.0,Operator "+",OpenParen,Number 1.5,Operator "*",Number 2.0,CloseParen,CloseParen,Operator "+",Number 4.95]
-- Right (((3.0 / 2.0) + (1.5 * 2.0)) + 4.95)
--
-- >>> parse [Number 9001.0,Operator "*",Number 29.12]
-- Right (9001.0 * 29.12)
--
-- >>> parse [Number 2.0,Identifier "km",OpenBracket,Identifier "m",CloseBracket]
-- Right 2.0 km[m]
--
-- >>> parse [Identifier "a",Equal,Number 3.0,Comma,Identifier "b",Equal,Number 2.0,Arrow,Identifier "a",Operator "+",Identifier "b"]
-- Right (a = 3.0, b = 2.0 -> (a + b))
--
-- >>> parse [Number 2.0,Operator "*",OpenParen,Number 3,Operator "+",Number 4,CloseParen,Operator "/", Number 7.0]
-- Right ((2.0 * (3.0 + 4.0)) / 7.0)
--
-- >>> parse [Number 2.0,OpenParen,Number 3,Operator "+",Number 4,CloseParen,Operator "/", Number 7.0]
-- Right ((2.0 * (3.0 + 4.0)) / 7.0)
--
module Math.SiConverter.Internal.Parser (parse) where

import Control.Applicative ((<|>))
import Control.Monad (liftM2, unless, void)
import Control.Monad.State

import Data.Bifunctor (first)

import GHC.Base (Alternative (empty))

import Math.SiConverter.Internal.AstProcessingSteps.Evaluate (execute, mergeUnits,
           subtractUnits)
import Math.SiConverter.Internal.DerivedUnits (derivedUnitFromString)
import Math.SiConverter.Internal.Expr
import Math.SiConverter.Internal.Lexer (Token (..), Tokens)
import Math.SiConverter.Internal.Operators (Op (..))
import Math.SiConverter.Internal.Units (Dimension, UnitExp (..), multiplier,
           unitFromString)
import Math.SiConverter.Internal.Utils.Composition ((.:))
import Math.SiConverter.Internal.Utils.Error (Error (..), Kind (ParseError))

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
            Left err      -> return $ Left err
            Right (f, ts) -> rhs ts >>= \case
                Left err         -> return $ Left err
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
parse :: Tokens            -- ^ Token stream
      -> Either Error Expr -- ^ Error message or parsed expression
parse tokens = case runParser parseExpr tokens of
    Right (result, []) -> Right result
    Right (_, ts)      -> Left $ Error ParseError $ "Parser was unable to parse the full input. " ++ show ts ++ " remains in the token stream."
    Left err           -> Left $ Error ParseError err

atLeastOne :: Parser a -> Token -> Parser [a]
atLeastOne p sep = do
    x <- p
    xs <- do {
        requireToken sep;
        atLeastOne p sep
    } <|> return []
    return $ x:xs

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

requireToken :: Token -> Parser ()
requireToken t = void $ satisfy (==t)

parseOperator :: Parser String
parseOperator = do
    Operator op <- satisfy isOperator
    return op

parseNumber :: Parser Double
parseNumber = do
    Number n <- satisfy isNumber
    return n

requireOperator :: String -> Parser ()
requireOperator op = do
    parsedOp <- parseOperator
    unless (parsedOp == op) $ fail $ "Expected operator " ++ op ++ " but got " ++ parsedOp

parseIdentifier :: Parser String
parseIdentifier = do
    Identifier i <- satisfy isIdentifier
    return i

parseDimension :: Parser Dimension
parseDimension = parseUnitExp >>= dim'
    where dim' parsedLhs = do {
        op <- parseFactorOp;
        parsedRhs <- parseUnitExp;
        case op of
            Mult -> dim' $ mergeUnits parsedLhs parsedRhs
            Div  -> dim' $ subtractUnits parsedLhs parsedRhs
            _ -> fail "Invalid factor operator"
    } <|> return parsedLhs


parseUnitExp :: Parser Dimension
parseUnitExp = do
    i <- parseIdentifier
    either (\x -> fail $ "Invalid unit " ++ x) (\dim -> do {
        requireOperator "^";
        expr <- parsePrimary;
        case execute expr of
            Right (Value v []) -> let e = round v :: Int in return ((\(UnitExp u e') -> UnitExp u $ e' * e) <$> dim)
            _                  -> fail "Exponentiation of units is not supported"
    } <|> return dim) $ parseUnitSymbol i

parseUnitSymbol :: String -> Either String Dimension
parseUnitSymbol i = do {
    simpleUnit <- unitFromString i;
    return [UnitExp simpleUnit 1]
  } <> derivedUnitFromString i

parseConversion :: Parser Dimension
parseConversion = requireToken OpenBracket *> parseDimension <* requireToken CloseBracket

parseExprInParens :: Parser Expr
parseExprInParens = requireToken OpenParen *> parseExpr <* requireToken CloseParen

parseUnaryOp :: Parser Op
parseUnaryOp = do
    requireOperator "-"
    return UnaryMinus

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
        "*" -> return Mult
        "/" -> return Div
        x   -> fail $ "Invalid binary operator " ++ x

parseExpr :: Parser Expr
parseExpr = do
    term <- parseVarBindings;
    Conversion term <$> parseConversion <|> return term


parseVarBindings :: Parser Expr
parseVarBindings = do {
    bs <- atLeastOne parseVarBindingHead Comma;
    requireToken Arrow;
    expr <- parseVarBindings;
    return $ VarBindings bs expr
  } <|> parseTerm

parseVarBindingHead :: Parser (String, Expr)
parseVarBindingHead = do {
    lhs <- parseIdentifier;
    requireToken Equal;
    rhs <- parseTerm;
    return (lhs, rhs)
  }

parseTerm :: Parser Expr
parseTerm = parseFactor >>= expr'
    where expr' parsedLhs = do {
         liftM2 (BinOp parsedLhs) parseTermOp parseFactor >>= expr'
    } <|> return parsedLhs

parseFactor :: Parser Expr
parseFactor = parsePower >>= factor'
    where factor' parsedLhs = do {
        liftM2 (BinOp parsedLhs) parseFactorOp parsePower >>= factor'
    } <|> (BinOp parsedLhs Mult <$> parseExprInParens >>= factor') <|> return parsedLhs

parsePower :: Parser Expr
parsePower = parseUnary >>= power'
    where power' parsedLhs = do {
        requireOperator "^";
        parsedRhs <- parseUnary;
        power' $ BinOp parsedLhs Pow parsedRhs
    } <|> return parsedLhs

parseUnary :: Parser Expr
parseUnary = liftM2 UnaryOp parseUnaryOp parsePrimary <|> parsePrimary

parsePrimary :: Parser Expr
parsePrimary = parseExprInParens <|> parseValue

parseValue :: Parser Expr
parseValue = liftM2 (Val .: Value) parseNumber (parseDimension <|> return (multiplier 1)) <|> Var <$> parseIdentifier
