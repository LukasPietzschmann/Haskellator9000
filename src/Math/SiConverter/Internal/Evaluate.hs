-- | Evaluate the expression tree
module Math.SiConverter.Internal.Evaluate (evaluate) where

import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.State (evalState, get, modify)

import Data.Map (insert, (!?))

import Math.SiConverter.Internal.Expr (AstFold, Expr (..), Op (..), Thunk (..),
           Value (..), partiallyFoldExprM)
import Math.SiConverter.Internal.Utils.Error (Error (Error), Kind (..))
import Math.SiConverter.Internal.Utils.Stack (mapTop, pop, push, top)

-- | Evaluate the expression tree. This requires all the units in the tree to be converted to their respective base units.
evaluate :: Expr                -- ^ the 'Expr' tree to evaluate
         -> Either Error Double -- ^ the resulting value
evaluate expr = evalState (runExceptT $ evaluate' expr) (push mempty mempty)

evaluate' :: Expr -> AstFold Double
evaluate' = partiallyFoldExprM (return . value) evalBinOp evalUnaryOp evalVarBind evalVar

evalBinOp :: Double -> Op -> Double -> AstFold Double
evalBinOp lhs Plus  rhs = return $ lhs + rhs
evalBinOp lhs Minus rhs = return $ lhs - rhs
evalBinOp lhs Mult  rhs = return $ lhs * rhs
evalBinOp lhs Div   rhs = return $ lhs / rhs
evalBinOp lhs Pow   rhs = return $ lhs ** rhs
evalBinOp _   op    _   = throwError $ Error ImplementationError $ "Unknown binary operator " ++ show op

evalUnaryOp :: Op -> Double -> AstFold Double
evalUnaryOp UnaryMinus rhs = return $ -rhs
evalUnaryOp op         _   = throwError $ Error ImplementationError $ "Unknown unary operator " ++ show op

evalVarBind :: String -> Expr -> Expr -> AstFold Double
evalVarBind lhs rhs expr = do
    modify $ push $ insert lhs (Expr rhs) mempty
    result <- evaluate' expr
    modify $ snd . pop
    return result

evalVar :: String -> AstFold Double
evalVar n = do
    context <- get
    case top context !? n of
        Just (Result v) -> return v
        Just (Expr e)   -> do
            result <- evaluate' e
            modify $ mapTop $ insert n $ Result result
            return result
        Nothing         -> throwError $ Error RuntimeError $ "Variable '" ++ n ++ "' not in scope"
