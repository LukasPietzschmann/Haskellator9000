{-# LANGUAGE FlexibleInstances, LambdaCase #-}

module Math.SiConverter.Internal.AstProcessingSteps.DetermineDimension (
      Dimension
    , determineDimension
    ) where

import Control.Monad.Except (MonadError (throwError))

import Data.List (intercalate)

import Math.SiConverter.Internal.Expr (Bindings, Expr (..), Op (..), SimpleAstFold,
           Thunk (..), Unit, UnitExp (UnitExp, dimUnit, power), Value (Value), bindVars,
           getVarBinding, isMultiplier, partiallyFoldExprM, runAstFold, runInNewScope)
import Math.SiConverter.Internal.Utils.Error (Error (..), Kind (..))

-- | The dimension of a quantity is given by a set of units raised to a power. Those
-- units are implicitly connected by multiplication.
type Dimension = [UnitExp]

instance {-# OVERLAPPING #-} Show Dimension where
    show xs = intercalate "*" (show <$> xs)

-- | Determines the resulting dimension of an expression tree. If you would evaluate the
-- expression tree, the numerical result has the dimension returned by this function.
determineDimension :: Expr                   -- ^ the 'Expr' tree to determine the resulting dimension of
                   -> Either Error Dimension -- ^ the resulting dimension
determineDimension = fmap (filterMultiplier . filterZeroPower) . runAstFold . determineDimension'
    where filterZeroPower = filter ((/=0) . power)
          filterMultiplier = filter (not . isMultiplier . dimUnit)

determineDimension' :: Expr -> SimpleAstFold Dimension
determineDimension' = partiallyFoldExprM (return . \(Value _ u) -> [UnitExp (dimUnit u) 1])
    determineDimensionBinOp
    determineDimensionUnaryOp
    determineDimensionConversion
    determineDimensionVarBind
    determineDimensionVar

determineDimensionBinOp :: Dimension -> Op -> Dimension -> SimpleAstFold Dimension
determineDimensionBinOp lhs Plus  rhs | lhs == rhs = return lhs
                                      | otherwise  = throwError $ Error RuntimeError "Addition of different units is not supported"
determineDimensionBinOp lhs Minus rhs | lhs == rhs = return lhs
                                      | otherwise  = throwError $ Error RuntimeError "Subtraction of different units is not supported"
determineDimensionBinOp lhs Mult  rhs = return $ mergeUnits lhs rhs
determineDimensionBinOp lhs Div   rhs = return $ subtractUnits lhs rhs
determineDimensionBinOp _   Pow   _   = throwError $ Error RuntimeError "Exponentiation of units is not supported"
determineDimensionBinOp _   op    _   = throwError $ Error ImplementationError $ "Unknown binary operator " ++ show op

determineDimensionUnaryOp :: Op -> Dimension -> SimpleAstFold Dimension
determineDimensionUnaryOp UnaryMinus d = return d
determineDimensionUnaryOp op         _ = throwError $ Error ImplementationError $ "Unknown unary operator " ++ show op

determineDimensionConversion :: Dimension -> Unit -> SimpleAstFold Dimension
determineDimensionConversion d _ = return d

determineDimensionVarBind :: Bindings Expr -> Expr -> SimpleAstFold Dimension
determineDimensionVarBind bs expr = do
    rhsDims <- mapM (determineDimension' . snd) bs
    let thunkBs = zipWith (\(n,_) d -> (n, Result d)) bs rhsDims
    runInNewScope $ bindVars thunkBs >> determineDimension' expr

determineDimensionVar :: String -> SimpleAstFold Dimension
determineDimensionVar n = getVarBinding n >>= \case
    (Result d) -> return d
    (Expr _)   -> throwError $ Error ImplementationError $ "Variable '" ++ n ++ "' not evaluated"

mergeUnits :: Dimension -> Dimension -> Dimension
mergeUnits [] ys = ys
mergeUnits xs [] = xs
mergeUnits (x:xs) (y:ys) | dimUnit x == dimUnit y = UnitExp (dimUnit x) (power x + power y) : mergeUnits xs ys
                         | otherwise              = x : mergeUnits xs (y:ys)

subtractUnits :: Dimension -> Dimension -> Dimension
subtractUnits [] ys = (\(UnitExp u e) -> UnitExp u $ e * (-1)) <$> ys
subtractUnits xs [] = xs
subtractUnits (x:xs) (y:ys) | dimUnit x == dimUnit y = UnitExp (dimUnit x) (power x - power y) : subtractUnits xs ys
                            | otherwise              = x : subtractUnits xs (y:ys)
