-- | Calculator module for evaluating mathematical expressions containing SI units

module Math.SiConverter (calculate) where

import Control.Monad ((>=>))

import Math.SiConverter.Internal.AstProcessingSteps.Evaluate
import Math.SiConverter.Internal.AstProcessingSteps.Normalize
import Math.SiConverter.Internal.Expr
import Math.SiConverter.Internal.Lexer
import Math.SiConverter.Internal.Parser
import Math.SiConverter.Internal.Units
import Math.SiConverter.Internal.Utils.Error

type EvalValue = Value [UnitExp]

getAst :: String -> Either Error Expr
getAst = scan >=> parse >=> normalize

-- | Evaluate a result from a given expression
calculate :: String                 -- ^ The expression to evaluate
          -> Either Error EvalValue -- ^ The result of the expression
calculate input = do
    ast <- getAst input
    evaluateWithConv ast

evaluateWithConv :: Expr -> Either Error EvalValue
evaluateWithConv = ev where
    ev (Conversion expr target) = do
        r <- execute expr
        let baseV = convertDimensionToBase r
        return $ convertDimensionTo baseV target
    ev expr                     = execute expr