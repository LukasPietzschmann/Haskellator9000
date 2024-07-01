{-# LANGUAGE FlexibleInstances #-}

-- | Evaluate the expression tree
module Math.SiConverter.Internal.Evaluator (
      Dimension
    , determineDimension
    , evaluate
    , normalize
    ) where

import Data.List (intercalate)

import Math.SiConverter.Internal.Expr (Expr (..), Op (..), Unit, Value (..),
           convertToBase, foldExpr, foldExprM, isMultiplier)
import Math.SiConverter.Internal.Utils.Error (Error (Error), Kind (ImplementationError))

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

-- TODO: implement custom fold, so we can handle var bindings correctly
-- | Determines the resulting dimension of an expression tree. If you would evaluate the
-- expression treee, the numerical result has the dimension returned by this function.
determineDimension :: Expr                   -- ^ the 'Expr' tree to determine the resulting dimension of
                   -> Either Error Dimension -- ^ the resulting dimension
determineDimension = fmap (filter (not . isMultiplier . dimUnit) . filter ((/=0) . power)) . foldExprM (\(Value _ u) -> return $ pure $ DimPart u 1) handleBinOp handleUnaryOp handleVarBinding
  where
    handleBinOp lhs Mult rhs  = return $ mergeUnits lhs rhs
    handleBinOp lhs Div rhs   = return $ subtractUnits lhs rhs
    handleBinOp _   Pow _     = Left $ Error ImplementationError "Exponentiation of units is not supported"
    handleBinOp lhs Plus rhs  = if lhs == rhs then return lhs else Left $ Error ImplementationError "Addition of different units is not supported"
    handleBinOp lhs Minus rhs = if lhs == rhs then return lhs else Left $ Error ImplementationError "Subtraction of different units is not supported"
    handleBinOp _   op _      = Left $ Error ImplementationError $ "Unknown binary operand " ++ show op
    handleUnaryOp UnaryMinus  = return
    handleUnaryOp op          = const $ Left $ Error ImplementationError $ "Unknown unary operand " ++ show op
    handleVarBinding _ _ _    = Left $ Error ImplementationError "Variable bindings are not yet supported"

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
normalize = Right . foldExpr (Val . convertToBase) BinOp UnaryOp VarBinding

-- TODO: implement custom eval fold, so we can handle var bindings correctly
-- | Evaluate the expression tree. This requires all the units in the tree to be converted to their respective base units.
evaluate :: Expr                -- ^ the 'Expr' tree to evaluate
         -> Either Error Double -- ^ the resulting value
evaluate = foldExprM (Right . value) evaluateBinOp evaluateUnaryOp evaluateVarBinding

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

evaluateVarBinding :: String -> Double -> Double -> Either Error Double
evaluateVarBinding _ _ = Right
