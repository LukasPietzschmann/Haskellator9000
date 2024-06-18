-- | Calculator module for evaluating mathematical expressions containing SI units

module Math.SiConverter(calculate) where

import Control.Monad((>=>))
import Math.SiConverter.Internal.Evaluator
import Math.SiConverter.Internal.Lexer
import Math.SiConverter.Internal.Parser

-- | Evaluate a result from a given expression
calculate :: String               -- ^ The expression to evaluate
          -> Either String Double -- ^ The numerical result of the expression
calculate input = scan input >>= (parse >=> normalize >=> evaluate)
