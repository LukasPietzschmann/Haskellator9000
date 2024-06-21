-- | Evaluate the expression tree
module Math.SiConverter.Internal.Evaluator (evaluate, normalize) where

import Math.SiConverter.Internal.Expr (Expr (..), Op (..), Value (..), convertToBase,
           foldExpr, foldExprM, value)
import Math.SiConverter.Internal.Utils.Error (Error (Error), Kind (ImplementationError))

-- | Normalize all values inside the tree to their base units
normalize :: Expr              -- | the 'Expr' tree to normalize
          -> Either Error Expr -- | the normalized 'Expr' tree
normalize = Right . foldExpr (Val . convertToBase) BinOp UnaryOp

-- | Evaluate the expression tree. This requires all the units in the tree to be converted to their respective base units.
evaluate :: Expr                -- | the 'Expr' tree to evaluate
         -> Either Error Double -- | the resulting value
evaluate = foldExprM (Right . value) evaluateBinOp evaluateUnaryOp

evaluateBinOp :: Double -> Op -> Double -> Either Error Double
evaluateBinOp = eval
  where
    eval v1 Plus v2 = Right $ v1 + v2
    eval v1 Minus v2 = Right $ v1 - v2
    eval v1 Mult v2 = Right $ v1 * v2
    eval v1 Div v2 = Right $ v1 / v2
    eval v1 Pow v2 = Right $ v1 ** v2
    eval _ op _ = Left $ Error ImplementationError $ "Unknown binary operand " ++ show op

evaluateUnaryOp :: Op -> Double -> Either Error Double
evaluateUnaryOp = eval
  where
    eval UnaryMinus v = Right $ -v
    eval op _ = Left $ Error ImplementationError $ "Unknown unary operand " ++ show op
