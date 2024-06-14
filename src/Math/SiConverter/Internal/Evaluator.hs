module Math.SiConverter.Internal.Evaluator (evaluate) where

import Math.SiConverter.Internal.Expr (Expr, Op (..), foldExpr)

evaluate :: Expr -> Double
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
    eval _ op _ = error $ "Unknown binary operand " ++ show op

evaluateUnaryOp :: Op -> Double -> Double
evaluateUnaryOp = eval
  where
    eval UnaryMinus v = -v
    eval op _ = error $ "Unknown unary operand " ++ show op
