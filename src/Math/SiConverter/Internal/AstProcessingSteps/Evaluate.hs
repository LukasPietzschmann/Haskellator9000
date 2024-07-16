{-# LANGUAGE LambdaCase #-}

-- | Evaluate the expression tree
module Math.SiConverter.Internal.AstProcessingSteps.Evaluate (
      evaluate
    , execute
    , mergeUnits
    , subtractUnits
    ) where

import Control.Monad.Except (throwError)

import Data.Functor ((<&>))

import Math.SiConverter.Internal.Expr (Bindings, Expr (..), SimpleAstFold, Thunk (..),
           Value (..), bindVar, bindVars, getVarBinding, partiallyFoldExprM, runAstFold,
           runInNewScope)
import Math.SiConverter.Internal.Operators (Op (..))
import Math.SiConverter.Internal.Units (Dimension, Unit (..), UnitExp (..),
           combineValues, isMultiplier, mapValue, (=~=))
import Math.SiConverter.Internal.Utils.Error (Error (Error), Kind (..))

evaluate :: Expr -> Either Error Double
evaluate expr = execute expr <&> value

execute :: Expr -> Either Error (Value Dimension)
execute expr = do
    r <- runAstFold $ execute' expr
    return $ r { unit = filterUnwanted $ unit r }

execute' :: Expr -> SimpleAstFold (Value Dimension)
execute' = partiallyFoldExprM execVal execBinOp execUnaryOp execConversion execVarBinds execVar

execVal :: Value Dimension -> SimpleAstFold (Value Dimension)
execVal = return

execBinOp :: Value Dimension -> Op -> Value Dimension -> SimpleAstFold (Value Dimension)
execBinOp lhs Plus  rhs | unit lhs =~= unit rhs = return $ combineValues (+) lhs rhs
                        | otherwise = throwError $ Error RuntimeError $ "Cannot add units " ++ show lhs ++ " and " ++ show rhs
execBinOp lhs Minus rhs | unit lhs =~= unit rhs = return $ combineValues (-) lhs rhs
                        | otherwise = throwError $ Error RuntimeError $ "Cannot subtract units " ++ show lhs ++ " and " ++ show rhs
execBinOp lhs Mult  rhs = do
    let u = mergeUnits (unit lhs) (unit rhs)
    return $ Value (value lhs * value rhs) u
execBinOp lhs Div   rhs = do
    let u = subtractUnits (unit lhs) (unit rhs)
    return $ Value (value lhs / value rhs) u
execBinOp lhs Pow   rhs = case rhs of
    Value _ [UnitExp Multiplier 1] -> return $ Value (value lhs ** value rhs) ((\u -> u {
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

mergeUnits :: Dimension -> Dimension -> Dimension
mergeUnits lhs rhs = [x{power = power x + power y} | (x, y) <- pairs] ++ lr ++ rr
    where (pairs, (lr, rr)) = findPairs lhs rhs

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
