{-# LANGUAGE FlexibleInstances #-}

-- | Evaluate the expression tree
module Math.SiConverter.Internal.Evaluator (
      Dimension
    , determineDimension
    , evaluate
    , normalize
    ) where

import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.State (evalState, get, modify)

import Data.List (intercalate)
import Data.Map (insert, (!?))

import Math.SiConverter.Internal.Expr (AstFold, Expr (..), Op (..), Thunk (..), Unit,
           Value (..), convertToBase, foldExpr, isMultiplier, partiallyFoldExprM)
import Math.SiConverter.Internal.Utils.Error (Error (Error), Kind (..))
import Math.SiConverter.Internal.Utils.Stack (mapTop, pop, push, top)

-- | A single unit constituting a dimension
data DimensionPart = DimPart { dimUnit :: Unit
                             , power   :: Int
                             }

instance Show DimensionPart where
    show (DimPart u p) = show u ++ (if p == 1 then "" else "^" ++ show p)

instance Eq DimensionPart where
    (DimPart u1 p1) == (DimPart u2 p2) = u1 == u2 && p1 == p2

-- | The dimension of a quantity is given by a set of units raised to a power. Those
-- units are implicitly connected by multiplication.
type Dimension = [DimensionPart]

instance {-# OVERLAPPING #-} Show Dimension where
    show xs = intercalate "*" (show <$> xs)

-- | Determines the resulting dimension of an expression tree. If you would evaluate the
-- expression tree, the numerical result has the dimension returned by this function.
determineDimension :: Expr                   -- ^ the 'Expr' tree to determine the resulting dimension of
                   -> Either Error Dimension -- ^ the resulting dimension
determineDimension expr = filter (not . isMultiplier . dimUnit) . filter ((/=0) . power) <$> evalState (runExceptT $ determineDimension' expr) (push mempty mempty)

determineDimension' :: Expr -> AstFold Dimension
determineDimension' = partiallyFoldExprM (return . (\v -> [DimPart (unit v) 1])) determineDimensionBinOp determineDimensionUnaryOp determineDimensionVarBind determineDimensionVar

determineDimensionBinOp :: Dimension -> Op -> Dimension -> AstFold Dimension
determineDimensionBinOp lhs Plus  rhs | lhs == rhs = return lhs
                                      | otherwise  = throwError $ Error RuntimeError "Addition of different units is not supported"
determineDimensionBinOp lhs Minus rhs | lhs == rhs = return lhs
                                      | otherwise  = throwError $ Error RuntimeError "Subtraction of different units is not supported"
determineDimensionBinOp lhs Mult  rhs = return $ mergeUnits lhs rhs
determineDimensionBinOp lhs Div   rhs = return $ subtractUnits lhs rhs
determineDimensionBinOp _   Pow   _   = throwError $ Error RuntimeError "Exponentiation of units is not supported"
determineDimensionBinOp _   op    _   = throwError $ Error ImplementationError $ "Unknown binary operator " ++ show op

determineDimensionUnaryOp :: Op -> Dimension -> AstFold Dimension
determineDimensionUnaryOp UnaryMinus d = return d
determineDimensionUnaryOp op         _ = throwError $ Error ImplementationError $ "Unknown unary operator " ++ show op

determineDimensionVarBind :: String -> Expr -> Expr -> AstFold Dimension
determineDimensionVarBind lhs rhs expr = do
    rhsDim <- determineDimension' rhs
    modify $ push $ insert lhs (Result rhsDim) mempty
    result <- determineDimension' expr
    modify $ snd . pop
    return result

determineDimensionVar :: String -> AstFold Dimension
determineDimensionVar n = do
    context <- get
    case top context !? n of
        Just (Result d) -> return d
        Just (Expr _)   -> throwError $ Error ImplementationError $ "Variable '" ++ n ++ "' not evaluated"
        Nothing         -> throwError $ Error RuntimeError $ "Variable '" ++ n ++ "' not in scope"

mergeUnits :: Dimension -> Dimension -> Dimension
mergeUnits [] ys = ys
mergeUnits xs [] = xs
mergeUnits (x:xs) (y:ys) | dimUnit x == dimUnit y = DimPart (dimUnit x) (power x + power y) : mergeUnits xs ys
                         | otherwise              = x : mergeUnits xs (y:ys)

subtractUnits :: Dimension -> Dimension -> Dimension
subtractUnits [] ys = (\d -> d{power = (-1) * power d}) <$> ys
subtractUnits xs [] = xs
subtractUnits (x:xs) (y:ys) | dimUnit x == dimUnit y = DimPart (dimUnit x) (power x - power y) : subtractUnits xs ys
                            | otherwise              = x : subtractUnits xs (y:ys)

-- | Normalize all values inside the tree to their base units
normalize :: Expr              -- ^ the 'Expr' tree to normalize
          -> Either Error Expr -- ^ the normalized 'Expr' tree
normalize = Right . foldExpr (Val . convertToBase) BinOp UnaryOp VarBinding Var

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
