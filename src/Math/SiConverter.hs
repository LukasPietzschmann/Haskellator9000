-- | Calculator module for evaluating mathematical expressions containing SI units

module Math.SiConverter(calculate) where

import Control.Monad((>=>))
import Math.SiConverter.Internal.Evaluator
import Math.SiConverter.Internal.Lexer
import Math.SiConverter.Internal.Parser
import Math.SiConverter.Internal.Utils.Error

-- | Evaluate a result from a given expression
calculate :: String               -- ^ The expression to evaluate
          -> Either Error Double  -- ^ The numerical result of the expression
calculate input = scan input >>= (parse >=> normalize >=> evaluate)
