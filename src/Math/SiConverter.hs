-- | Calculator module for evaluating mathematical expressions containing SI units

module Math.SiConverter (calculate) where

import Control.Monad ((>=>))

import Math.SiConverter.Internal.AstProcessingSteps.DetermineDimension
import Math.SiConverter.Internal.AstProcessingSteps.Evaluate
import Math.SiConverter.Internal.AstProcessingSteps.Normalize
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
    dim <- determineDimension ast
    evaluateWithConv ast dim

evaluateWithConv :: Expr -> Dimension -> Either Error EvalValue
evaluateWithConv = ev where
    ev (Conversion expr newUnit) ((UnitExp oldUnit e):_)  = evaluate expr >>= \r -> return $ Value (value (result r)) [unit (result r)]
        where
            result r = convertTo (Value r (UnitExp oldUnit e)) newUnit
    ev expr dim                               = evaluate expr >>= \r -> return $ Value r dim