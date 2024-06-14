-- | Calculator module for evaluating mathematical expressions using SI units
module Math.SiConverter(calculate) where

import Math.SiConverter.Internal.Evaluator
import Math.SiConverter.Internal.Lexer
import Math.SiConverter.Internal.Parser

-- | Evaluate a result from a given expression
calculate ::
  -- Mathematical expression
  String ->
  -- Result
  Double
calculate = evaluate . parse . scan
