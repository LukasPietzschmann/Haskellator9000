-- | Recursive decent parser
module Math.SiConverter.Parser (
  parse,
  -- TODO Remove this temporary export when expression evaluation is supported
  Expr) where

import Math.SiConverter.Lexer (Token(..), Tokens)
import Math.SiConverter.Expr

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

parsePrimary :: Tokens -> (Expr, Tokens)
parsePrimary (Number n:ts) = (Value n Placeholder, ts)
parsePrimary (Identifier _:_) = error "We don't yet need idenfifiers"
parsePrimary (OpenParen:ts) = case parseTerm ts of
  (e, CloseParen:ts') -> (e, ts')
  _ -> error "Expected closing parenthesis"
parsePrimary r = error $ "Expected number, identifier, or open parenthesis, but got " ++ show r
