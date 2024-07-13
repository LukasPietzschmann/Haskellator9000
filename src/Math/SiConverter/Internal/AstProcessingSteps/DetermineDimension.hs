{-# LANGUAGE FlexibleInstances, LambdaCase #-}

module Math.SiConverter.Internal.AstProcessingSteps.DetermineDimension (
      Dimension
    , determineDimension
    ) where

import Control.Monad.Except (MonadError (throwError))

import Data.List (intercalate)

import Math.SiConverter.Internal.Expr (Bindings, Expr (..), SimpleAstFold, Thunk (..),
           Value (Value), bindVars, getVarBinding, partiallyFoldExprM, runAstFold,
           runInNewScope)
import Math.SiConverter.Internal.Operators (Op (..))
import Math.SiConverter.Internal.Units (Unit (..), UnitExp (..), isMultiplier)
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
determineDimension = fmap filterUnwanted . runAstFold . determineDimension'

determineDimension' :: Expr -> SimpleAstFold Dimension
determineDimension' = partiallyFoldExprM (return . \(Value _ u) -> [UnitExp (dimUnit u) (power u)])
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
determineDimensionBinOp lhs Pow   rhs | rhs == [UnitExp Multiplier 1] = return $ fmap (\u -> u {
    power = power u * 1 -- TODO: evaluate rhs and use the result as the multiplier
  }) lhs
                                      | otherwise                     = throwError $ Error RuntimeError "Exponentiation of units is not supported"
determineDimensionBinOp _   op    _   = throwError $ Error ImplementationError $ "Unknown binary operator " ++ show op

determineDimensionUnaryOp :: Op -> Dimension -> SimpleAstFold Dimension
determineDimensionUnaryOp UnaryMinus d = return d
determineDimensionUnaryOp op         _ = throwError $ Error ImplementationError $ "Unknown unary operator " ++ show op

determineDimensionConversion :: Dimension -> UnitExp -> SimpleAstFold Dimension
determineDimensionConversion src (UnitExp _ e') = case filterUnwanted src of
    [UnitExp u e] | e == e'   -> return $ pure $ UnitExp u e'
                  | otherwise -> throwError $ Error RuntimeError "Conversion of different units is not supported"
    _                         -> throwError $ Error RuntimeError "Conversion of different units is not supported"

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

filterUnwanted :: Dimension -> Dimension
filterUnwanted = filterZeroPower . filterMultiplier

filterZeroPower :: Dimension -> Dimension
filterZeroPower = filter ((/=0) . power)

filterMultiplier :: Dimension -> Dimension
filterMultiplier = filter (not . isMultiplier . dimUnit)
