{-# LANGUAGE LambdaCase #-}
-- | Evaluate the expression tree
module Math.SiConverter.Internal.AstProcessingSteps.Evaluate (evaluate) where

import Control.Monad.Except (throwError)

import Math.SiConverter.Internal.Expr (Bindings, Expr (..), SimpleAstFold, Thunk (..),
           Value (..), bindVar, bindVars, getVarBinding, partiallyFoldExprM, runAstFold,
           runInNewScope)
import Math.SiConverter.Internal.Operators (Op (..))
import Math.SiConverter.Internal.Units (UnitExp)
import Math.SiConverter.Internal.Utils.Composition ((.:))
import Math.SiConverter.Internal.Utils.Error (Error (Error), Kind (..))

-- | Evaluate the expression tree. This requires all the units in the tree to be converted to their respective base units.
evaluate :: Expr                -- ^ the 'Expr' tree to evaluate
         -> Either Error Double -- ^ the resulting value
evaluate = runAstFold . evaluate'

evaluate' :: Expr -> SimpleAstFold Double
evaluate' = partiallyFoldExprM (return . value) evalBinOp evalUnaryOp evalConversion evalVarBinds evalVar

evalBinOp :: Double -> Op -> Double -> SimpleAstFold Double
evalBinOp lhs Plus  rhs = return $ lhs + rhs
evalBinOp lhs Minus rhs = return $ lhs - rhs
evalBinOp lhs Mult  rhs = return $ lhs * rhs
evalBinOp lhs Div   rhs = return $ lhs / rhs
evalBinOp lhs Pow   rhs = return $ lhs ** rhs
evalBinOp _   op    _   = throwError $ Error ImplementationError $ "Unknown binary operator " ++ show op

evalUnaryOp :: Op -> Double -> SimpleAstFold Double
evalUnaryOp UnaryMinus rhs = return $ -rhs
evalUnaryOp op         _   = throwError $ Error ImplementationError $ "Unknown unary operator " ++ show op

evalConversion :: Double -> [UnitExp] -> SimpleAstFold Double
evalConversion = return .: const

evalVarBinds :: Bindings Expr -> Expr -> SimpleAstFold Double
evalVarBinds bs expr = runInNewScope $ do
    bindVars $ fmap Expr <$> bs
    evaluate' expr

evalVar :: String -> SimpleAstFold Double
evalVar n = getVarBinding n >>= \case
    (Result v) -> return v
    (Expr e)   -> do
        result <- evaluate' e
        bindVar n $ Result result
        return result
