module Math.SiConverter.Evaluator where

import Math.SiConverter.Internal.Expr (foldExpr,Op(..),Expr)

evaluate::Expr->Double
evaluate = foldExpr (\v _ -> v) 
                    evaluateBinOp
                    (\_ _ -> error "Not yet implemented")

evaluateBinOp::Double->Op->Double->Double
evaluateBinOp v1 Plus v2 = v1 + v2
evaluateBinOp v1 Minus v2 = v1 - v2
evaluateBinOp v1 Mult v2 = v1 * v2
evaluateBinOp v1 Div v2 = v1 / v2

