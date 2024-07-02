{-# LANGUAGE FlexibleInstances #-}

-- | Evaluate the expression tree
module Math.SiConverter.Internal.Evaluator (
      Dimension
    , determineDimension
    , evaluate
    , normalize
    ) where

import Control.Monad.State (State, evalState, get, modify)

import Data.List (intercalate)
import Data.Map (Map, insert, (!?))

import Math.SiConverter.Internal.Expr (Expr (..), Op (..), Unit, Value (..),
           convertToBase, foldExpr, isMultiplier)
import Math.SiConverter.Internal.Utils.Error (Error)

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
determineDimension = return . (filter (not . isMultiplier . dimUnit) . filter ((/=0) . power)) . flip evalState mempty . determineDimension'

determineDimension' :: Expr -> State (Map String Dimension) Dimension
determineDimension' (Val (Value _ u)) = return [DimPart u 1]
determineDimension' (BinOp lhs op rhs) = do
    l <- determineDimension' lhs
    r <- determineDimension' rhs
    case op of
        Mult  -> return $ mergeUnits l r
        Div   -> return $ subtractUnits l r
        Plus  -> if l == r then return l else error "Addition of different units is not supported"
        Minus -> if l == r then return l else error "Subtraction of different units is not supported"
        _     -> error $ "Unknown binary operator " ++ show op
determineDimension' (UnaryOp op e) = do
    d <- determineDimension' e
    case op of
        UnaryMinus -> return d
        _          -> error $ "Unknown unary operator " ++ show op
determineDimension' (VarBinding lhs rhs expr) = do
    rhsv <- determineDimension' rhs
    modify $ insert lhs rhsv
    determineDimension' expr
determineDimension' (Var n) = do
    context <- get
    case context !? n of
        Just d  -> return d
        Nothing -> error $ "Variable " ++ n ++ " not in scope"

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
evaluate = return . flip evalState mempty . evaluate'

evaluate' :: Expr -> State (Map String Double) Double
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
        _     -> error $ "Unknown binary operator " ++ show op
evaluate' (UnaryOp op e) = do
    v <- evaluate' e
    case op of
        UnaryMinus -> return $ -v
        _          -> error $ "Unknown unary operator " ++ show op
evaluate' (VarBinding lhs rhs expr) = do
    rhsv <- evaluate' rhs
    modify $ insert lhs rhsv
    evaluate' expr
evaluate' (Var n) = do
    context <- get
    case context !? n of
        Just v  -> return v
        Nothing -> error $ "Variable " ++ n ++ " not in scope"
