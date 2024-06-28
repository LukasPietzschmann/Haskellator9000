-- | Calculator module for evaluating mathematical expressions containing SI units

module Math.SiConverter (calculate) where

import Control.Monad (liftM2, (>=>))

import Math.SiConverter.Internal.Evaluator
import Math.SiConverter.Internal.Expr
import Math.SiConverter.Internal.Lexer
import Math.SiConverter.Internal.Parser
import Math.SiConverter.Internal.Utils.Error

type EvalValue = Value Dimension

getAst :: String -> Either Error Expr
getAst = scan >=> parse >=> normalize

-- | Evaluate a result from a given expression
calculate :: String                 -- ^ The expression to evaluate
          -> Either Error EvalValue -- ^ The result of the expression
calculate input = do
    ast <- getAst input
    liftM2 Value (evaluate ast) (determineDimension ast)
