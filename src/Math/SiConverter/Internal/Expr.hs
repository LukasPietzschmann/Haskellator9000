{-# LANGUAGE TemplateHaskell #-}

-- | Models an expression tree
--
-- Examples:
--
-- >>> show $ BinOp (Val $ Value 1.0 $ Meter 1) Plus (BinOp (Val $ Value 2 $ Multiplier 1) Mult (Val $ Value 3.0 $ Meter 1))
-- "(1.0m + (2.0 * 3.0m))"
--
-- >>> show $ BinOp (BinOp (Val $ Value 1.0 $ Meter 1) Plus (Val $ Value 2.0 $ Meter 1)) Mult (Val $ Value 3.0 $ Multiplier 1)
-- "((1.0m + 2.0m) * 3.0)"

module Math.SiConverter.Internal.Expr (
      AstFold
    , AstValue
    , Expr (..)
    , Op (..)
    , Thunk (..)
    , Unit (..)
    , Value (..)
    , convertToBase
    , foldExpr
    , isMultiplier
    , partiallyFoldExprM
    , unitFromString
    ) where

import Control.Monad.Except (ExceptT)
import Control.Monad.State (State)

import Data.Map (Map)

import Math.SiConverter.Internal.TH.UnitGeneration (OperatorDef (..), Quantity (..),
           UnitDef (..), generateOperators, generateUnits)
import Math.SiConverter.Internal.Utils.Error (Error)
import Math.SiConverter.Internal.Utils.Stack (Stack)

$(generateUnits [
    Quantity (UnitDef "Multiplier" "" 1) [],
    Quantity (UnitDef "Meter" "m" 1) [],
    Quantity (UnitDef "Second" "s" 1) [],
    Quantity (UnitDef "Kilo" "kg" 1) []
  ])

$(generateOperators [
    OperDef "Plus" "+",
    OperDef "Minus" "-",
    OperDef "Mult" "*",
    OperDef "Div" "/",
    OperDef "Pow" "^",
    OperDef "UnaryMinus" "-"
  ])

type AstValue = Value Unit

data Expr = Val AstValue
          | BinOp Expr Op Expr
          | UnaryOp Op Expr
          | VarBinding String Expr Expr
          | Var String

data Thunk a = Expr Expr
             | Result a

-- | Folds an expression tree
foldExpr :: (AstValue -> a)         -- ^ function that folds a value
         -> (a -> Op -> a -> a)     -- ^ function that folds a binary expression
         -> (Op -> a -> a)          -- ^ function that folds a unary expression
         -> (String -> a -> a -> a) -- ^ function that folds a variable binding
         -> (String -> a)           -- ^ function that folds a variable
         -> Expr                    -- ^ the 'Expr' to fold over
         -> a                       -- ^ the resulting value
foldExpr fv fb fu fvb fvn = doIt
  where
    doIt (Val v)            = fv v
    doIt (BinOp e1 o e2)    = fb (doIt e1) o (doIt e2)
    doIt (UnaryOp o e)      = fu o $ doIt e
    doIt (VarBinding l r e) = fvb l (doIt r) (doIt e)
    doIt (Var n)            = fvn n

-- | Encapsulates the result 'a' of folding an expression tree and holds the current
-- state of variable bindings
type AstFold a = ExceptT Error (State (Stack (Map String (Thunk a)))) a

-- | Like 'foldExpr', but does not fold into variable bindings and returns a monadic
-- result
partiallyFoldExprM :: (AstValue -> AstFold a) -> (a -> Op -> a -> AstFold a) -> (Op -> a -> AstFold a) -> (String -> Expr -> Expr -> AstFold a) -> (String -> AstFold a) -> Expr -> AstFold a
partiallyFoldExprM fv fb fu fbv fvar = doIt
    where
        doIt (Val v) = fv v
        doIt (BinOp lhs op rhs) = do
            l <- doIt lhs
            r <- doIt rhs
            fb l op r
        doIt (UnaryOp op e) = do
            v <- doIt e
            fu op v
        doIt (VarBinding lhs rhs expr) = fbv lhs rhs expr
        doIt (Var n) = fvar n

instance Eq Expr where
    e1 == e2 = show e1 == show e2

instance Show Expr where
    show = foldExpr show showBinOp showUnaryOp showVarBind id
      where
        showBinOp e1 o e2 = "(" ++ e1 ++ " " ++ show o ++ " " ++ e2 ++ ")"
        showUnaryOp o e = "(" ++ show o ++ e ++ ")"
        showVarBind l r e = "(" ++ l ++ " = " ++ r ++ " -> " ++ e ++ ")"
