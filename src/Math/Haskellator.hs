-- | Calculator module for evaluating mathematical expressions containing SI units

module Math.Haskellator (EvalValue, calculate) where

import Control.Monad

import Math.Haskellator.Internal.AstProcessingSteps.Evaluate
import Math.Haskellator.Internal.AstProcessingSteps.Normalize
import Math.Haskellator.Internal.Expr
import Math.Haskellator.Internal.Lexer
import Math.Haskellator.Internal.Parser
import Math.Haskellator.Internal.Units
import Math.Haskellator.Internal.Utils.Error

type EvalValue = Value Dimension

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
        case tryConvertDimensionTo baseV target of
            Just v  -> return v
            Nothing -> Left $ Error RuntimeError $ "Cannot convert " ++ show baseV ++ " to " ++ show target
    ev expr                     = execute expr