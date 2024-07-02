{-# LANGUAGE FlexibleInstances #-}

-- | Evaluate the expression tree
module Math.SiConverter.Internal.Evaluator (
      Dimension
    , determineDimension
    , evaluate
    , normalize
    ) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, evalState, get, modify)

import Data.List (intercalate)
import Data.Map (Map, insert, (!?))

import Math.SiConverter.Internal.Expr (Expr (..), Op (..), Thunk (..), Unit, Value (..),
           convertToBase, foldExpr, isMultiplier)
import Math.SiConverter.Internal.Utils.Error (Error (Error), Kind (..))
import Math.SiConverter.Internal.Utils.Stack (Stack, mapTop, pop, push, top)

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

determineDimension' :: Expr -> ExceptT Error (State (Stack (Map String (Thunk Dimension)))) Dimension
determineDimension' (Val (Value _ u)) = return [DimPart u 1]
determineDimension' (BinOp lhs op rhs) = do
    l <- determineDimension' lhs
    r <- determineDimension' rhs
    case op of
        Mult  -> return $ mergeUnits l r
        Div   -> return $ subtractUnits l r
        Plus  -> if l == r then return l else error "Addition of different units is not supported"
        Minus -> if l == r then return l else error "Subtraction of different units is not supported"
        _     -> throwError $ Error ImplementationError $ "Unknown binary operator " ++ show op
determineDimension' (UnaryOp op e) = do
    d <- determineDimension' e
    case op of
        UnaryMinus -> return d
        _          -> throwError $ Error ImplementationError $ "Unknown unary operator " ++ show op
determineDimension' (VarBinding lhs rhs expr) = do
    modify $ push $ insert lhs (Expr rhs) mempty
    result <- determineDimension' expr
    modify $ snd . pop
    return result
determineDimension' (Var n) = do
    context <- get
    case top context !? n of
        Just (Result d) -> return d
        Just (Expr e)   -> do
            result <- determineDimension' e
            modify $ mapTop $ insert n $ Result result
            return result
        Nothing          -> throwError $ Error RuntimeError $ "Variable '" ++ n ++ "' not in scope"

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

evaluate' :: Expr -> ExceptT Error (State (Stack (Map String (Thunk Double)))) Double
evaluate' (Val v) = return $ value v
evaluate' (BinOp lhs op rhs) = do
    v1 <- evaluate' lhs
    v2 <- evaluate' rhs
    case op of
        Plus  -> return $ v1 + v2
        Minus -> return $ v1 - v2
        Mult  -> return $ v1 * v2
        Div   -> return $ v1 / v2
        Pow   -> return $ v1 ** v2
        _     -> throwError $ Error ImplementationError $ "Unknown binary operator " ++ show op
evaluate' (UnaryOp op e) = do
    v <- evaluate' e
    case op of
        UnaryMinus -> return $ -v
        _          -> throwError $ Error ImplementationError $ "Unknown unary operator " ++ show op
evaluate' (VarBinding lhs rhs expr) = do
    modify $ push $ insert lhs (Expr rhs) mempty
    result <- evaluate' expr
    modify $ snd . pop
    return result
evaluate' (Var n) = do
    context <- get
    case top context !? n of
        Just (Result v) -> return v
        Just (Expr e)   -> do
            result <- evaluate' e
            modify $ mapTop $ insert n $ Result result
            return result
        Nothing         -> throwError $ Error RuntimeError $ "Variable '" ++ n ++ "' not in scope"
