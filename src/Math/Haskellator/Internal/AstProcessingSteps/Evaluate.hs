{-# LANGUAGE LambdaCase #-}

-- | Evaluate the expression tree to a numeric value and a dimension.

module Math.Haskellator.Internal.AstProcessingSteps.Evaluate (
      evaluate
    , execute
    , mergeUnits
    , subtractUnits
    ) where

import Control.Monad.Except

import Data.Functor

import Math.Haskellator.Internal.Expr
import Math.Haskellator.Internal.Operators
import Math.Haskellator.Internal.Units
import Math.Haskellator.Internal.Utils.Error

-- | Evaluate an expression tree to a numeric value
evaluate :: Expr                -- ^ the expression tree to evaluate
         -> Either Error Double -- ^ the numeric result or an error
evaluate expr = execute expr <&> value

-- | Determine the result (value and dimension) of an expression tree
execute :: Expr                           -- ^ the expression tree to evaluate
        -> Either Error (Value Dimension) -- ^ the result or an error
execute expr = do
    r <- runAstFold $ execute' expr
    return $ r { unit = filterUnwanted $ unit r }

execute' :: Expr -> SimpleAstFold (Value Dimension)
execute' = partiallyFoldExprM execVal execBinOp execUnaryOp execConversion execVarBinds execVar

execVal :: Value Dimension -> SimpleAstFold (Value Dimension)
execVal = return

execBinOp :: Value Dimension -> Op -> Value Dimension -> SimpleAstFold (Value Dimension)
execBinOp lhs Plus  rhs | unit lhs =~= unit rhs = return $ combineValues (+) lhs rhs
                        | otherwise = throwError $ Error RuntimeError $ "Cannot add units " ++ show (unit lhs) ++ " and " ++ show (unit rhs)
execBinOp lhs Minus rhs | unit lhs =~= unit rhs = return $ combineValues (-) lhs rhs
                        | otherwise = throwError $ Error RuntimeError $ "Cannot subtract units " ++ show (unit lhs) ++ " and " ++ show (unit rhs)
execBinOp lhs Mult  rhs = do
    let u = mergeUnits (unit lhs) (unit rhs)
    return $ Value (value lhs * value rhs) u
execBinOp lhs Div   rhs = do
    let u = subtractUnits (unit lhs) (unit rhs)
    return $ Value (value lhs / value rhs) u
execBinOp lhs Pow   rhs = case rhs of
    Value _ [UnitExp Multiplier _] -> return $ Value (value lhs ** value rhs) ((\u -> u {
        power = power u * (round (value rhs) :: Int)
      }) <$> unit lhs)
    _                              -> throwError $ Error RuntimeError "Exponentiation of units is not supported"
execBinOp _   op    _   = throwError $ Error ImplementationError $ "Unknown binary operator " ++ show op

execUnaryOp :: Op -> Value Dimension -> SimpleAstFold (Value Dimension)
execUnaryOp op rhs = case op of
    UnaryMinus -> return $ mapValue (0-) rhs
    _          -> throwError $ Error ImplementationError $ "Unknown unary operator " ++ show op

execConversion :: Value Dimension -> Dimension -> SimpleAstFold (Value Dimension)
execConversion _ _ = throwError $ Error ImplementationError "Conversion is handled elsewhere"

execVarBinds :: Bindings Expr -> Expr -> SimpleAstFold (Value Dimension)
execVarBinds bs expr = runInNewScope $ do
    bindVars $ fmap Expr <$> bs
    execute' expr

execVar :: String -> SimpleAstFold (Value Dimension)
execVar n = getVarBinding n >>= \case
    (Result v) -> return v
    (Expr e)   -> do
        result <- execute' e
        bindVar n $ Result result
        return result

-- | Combine the Units of two Dimensions, by adding the powers of matching units.
-- >>> mergeUnits [UnitExp Meter 2, UnitExp Second 1, UnitExp Kilogram 1] [UnitExp Meter 1, UnitExp Second (-2)]
-- m^3*kg/s
mergeUnits :: Dimension -> Dimension -> Dimension
mergeUnits lhs rhs = [x{power = power x + power y} | (x, y) <- pairs] ++ lr ++ rr
    where (pairs, (lr, rr)) = findPairs lhs rhs

-- | Combine the Units of two Dimensions, by subtracting the powers of matching units.
-- >>> subtractUnits [UnitExp Meter 2, UnitExp Second 1, UnitExp Kilogram 1] [UnitExp Meter 1, UnitExp Second (-2)]
-- m*s^3*kg
subtractUnits :: Dimension -> Dimension -> Dimension
subtractUnits lhs rhs = [x{power = power x - power y} | (x, y) <- pairs] ++ lr ++ fmap flipPower rr
    where (pairs, (lr, rr)) = findPairs lhs rhs
          flipPower (UnitExp d e) = UnitExp d (-e)

-- | Finds pairs of 'UniExp's with the same 'Unit' in two 'Dimension's. Returns the pairs
--   and the remaining 'Dimension's split into dimensions from the left and right hand side.
findPairs :: Dimension -> Dimension -> ([(UnitExp, UnitExp)], (Dimension, Dimension))
findPairs [] ys = ([], ([], ys))
findPairs (x:xs) ys = let (pairs, (lr', rr')) = findPairs xs rr in (pair ++ pairs, (lr ++ lr', rr'))
    where (pair, (lr, rr)) = findPair x ys

-- | Finds a single pair of 'UnitExp's with the same 'Unit' in a 'Dimension'. Returns the pair
--   and the remaining 'Dimension' split into dimensions from the left and right hand side.
findPair :: UnitExp -> Dimension -> ([(UnitExp, UnitExp)], (Dimension, Dimension))
findPair x [] = ([], ([x], []))
findPair x (y:ys) | dimUnit x == dimUnit y = ([(x, y)], ([], ys))
                  | otherwise              = let (pair, (lr, rr)) = findPair x ys
                                              in (pair, (lr, y:rr))

filterUnwanted :: Dimension -> Dimension
filterUnwanted = filterZeroPower . filterMultiplier

filterZeroPower :: Dimension -> Dimension
filterZeroPower = filter ((/=0) . power)

filterMultiplier :: Dimension -> Dimension
filterMultiplier = filter (not . isMultiplier . dimUnit)
