-- | Calculator module for evaluating mathematical expressions using SI units
module Math.SiConverter where

import Math.SiConverter.Lexer
import Math.SiConverter.Parser

-- | Evaluate a result from a given expression
calculate ::
  -- Mathematical expression
  String ->
  -- Resulting syntax tree
  Expr
calculate = parse . scan
