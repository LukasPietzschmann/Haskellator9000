-- | Recursive decent parser
module Math.SiConverter.Internal.Parser (
  parse,
  -- TODO Remove this temporary export when expression evaluation is supported
  Expr) where

import Math.SiConverter.Internal.Lexer (Token(..), Tokens)
import Math.SiConverter.Internal.Expr

-- | Parse a token stream to an expression tree
--
-- Examples:
--
-- >>> parse [Number 1.0,Operator "+",Number 2.0]
-- (1.0 + 2.0)
--
-- >>> parse [OpenParen,Number 3.0,Operator "/",Number 2.0,Operator "+",OpenParen,Number 1.5,Operator "*",Number 2.0,CloseParen,CloseParen,Operator "+",Number 4.95]
-- ((3.0 / (2.0 + (1.5 * 2.0))) + 4.95)
--
-- >>> parse [Number 9001.0,Operator "*",Number 29.12]
-- (9001.0 * 29.12)
parse :: Tokens -> Expr
parse ts = case parseTerm ts of
  (e, []) -> e
  (_, ts') -> error $ "Unexpected tokens: " ++ show ts'

parseTerm :: Tokens -> (Expr, Tokens)
parseTerm ts = case parseFactor ts of
  (e1, Operator "*" : ts') -> case parseTerm ts' of
    (e2, ts'') -> (BinOp e1 Mult e2, ts'')
  (e1, Operator "/" : ts') -> case parseTerm ts' of
    (e2, ts'') -> (BinOp e1 Div e2, ts'')
  f -> f

parseFactor :: Tokens -> (Expr, Tokens)
parseFactor ts = case parsePrimary ts of
  (e1, Operator "+" : ts') -> case parseTerm ts' of
    (e2, ts'') -> (BinOp e1 Plus e2, ts'')
  (e1, Operator "-" : ts') -> case parseTerm ts' of
    (e2, ts'') -> (BinOp e1 Minus e2, ts'')
  p -> p

-- TODO Add identifier support
parsePrimary :: Tokens -> (Expr, Tokens)
parsePrimary (Number n:ts) = (Value n Multiplier, ts)
parsePrimary (Identifier _:_) = error "We don't yet need idenfifiers"
parsePrimary (OpenParen:ts) = case parseTerm ts of
  (e, CloseParen:ts') -> (e, ts')
  _ -> error "Expected closing parenthesis"
parsePrimary r = error $ "Expected number, identifier, or open parenthesis, but got " ++ show r
