-- | Evaluate the expression tree

module Math.SiConverter.Internal.Evaluator (evaluate) where

import Math.SiConverter.Internal.Expr (Expr, Op (..), foldExpr)

-- | Evaluate the expression tree. This requires all the units in the tree to be converted to their respective base units.
evaluate :: Expr   -- ^ the 'Expr' tree to evaluate
         -> Double -- ^ the resulting value
evaluate =
  foldExpr
    const
    evaluateBinOp
    evaluateUnaryOp

evaluateBinOp :: Double -> Op -> Double -> Double
evaluateBinOp = eval
  where
    eval v1 Plus v2 = v1 + v2
    eval v1 Minus v2 = v1 - v2
    eval v1 Mult v2 = v1 * v2
    eval v1 Div v2 = v1 / v2
    eval v1 Pow v2 = v1 ** v2
    eval _ op _ = error $ "Unknown binary operand " ++ show op

evaluateUnaryOp :: Op -> Double -> Double
evaluateUnaryOp = eval
  where
    eval UnaryMinus v = -v
    eval op _ = error $ "Unknown unary operand " ++ show op
