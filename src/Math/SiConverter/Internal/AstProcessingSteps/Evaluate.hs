-- | Evaluate the expression tree
module Math.SiConverter.Internal.AstProcessingSteps.Evaluate (evaluate) where

import Control.Monad.Except (throwError)
import Control.Monad.State (get, modify)

import Data.Map (insert, (!?))

import Math.SiConverter.Internal.Expr (Expr (..), Op (..), SimpleAstFold, Thunk (..),
           Value (..), bindVar, partiallyFoldExprM, runAstFold, runInNewScope)
import Math.SiConverter.Internal.Utils.Error (Error (Error), Kind (..))
import Math.SiConverter.Internal.Utils.Stack (mapTop, top)

-- | Evaluate the expression tree. This requires all the units in the tree to be converted to their respective base units.
evaluate :: Expr                -- ^ the 'Expr' tree to evaluate
         -> Either Error Double -- ^ the resulting value
evaluate = runAstFold . evaluate'

evaluate' :: Expr -> SimpleAstFold Double
evaluate' = partiallyFoldExprM (return . value) evalBinOp evalUnaryOp evalVarBind evalVar

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

evalVarBind :: String -> Expr -> Expr -> SimpleAstFold Double
evalVarBind lhs rhs expr = runInNewScope $ do
    bindVar lhs $ Expr expr
    evaluate' rhs

evalVar :: String -> SimpleAstFold Double
evalVar n = do
    context <- get
    case top context !? n of
        Just (Result v) -> return v
        Just (Expr e)   -> do
            result <- evaluate' e
            modify $ mapTop $ insert n $ Result result
            return result
        Nothing         -> throwError $ Error RuntimeError $ "Variable '" ++ n ++ "' not in scope"
