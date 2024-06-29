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
      AstValue
    , Expr (..)
    , Op (..)
    , Unit (..)
    , Value (..)
    , convertToBase
    , foldExpr
    , foldExprM
    , unitFromString
    ) where

import Math.SiConverter.Internal.TH.UnitGeneration (OperatorDef (..), Quantity (..),
           UnitDef (..), generateOperators, generateUnits)

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

-- | Folds an expression tree
foldExpr :: (AstValue -> a)     -- ^ function that folds a value
         -> (a -> Op -> a -> a) -- ^ function that folds a binary expression
         -> (Op -> a -> a)      -- ^ function that folds a unary expression
         -> Expr                -- ^ the 'Expr' to fold over
         -> a                   -- ^ the resulting value
foldExpr fv fb fu = doIt
  where
    doIt (Val v)         = fv v
    doIt (BinOp e1 o e2) = fb (doIt e1) o (doIt e2)
    doIt (UnaryOp o e)   = fu o $ doIt e

-- | Monadic fold over an expression tree
foldExprM :: (Monad m) => (AstValue -> m a) -- ^ function that folds a value
         -> (a -> Op -> a -> m a)           -- ^ function that folds a binary expression
         -> (Op -> a -> m a)                -- ^ function that folds a unary expression
         -> Expr                            -- ^ the 'Expr' to fold over
         -> m a                             -- ^ the resulting value
foldExprM fv fb fu = doIt
  where
    doIt (Val v) = fv v
    doIt (BinOp e1 o e2) = do
        v1 <- doIt e1
        v2 <- doIt e2
        fb v1 o v2
    doIt (UnaryOp o e) = doIt e >>= \v -> fu o v

instance Eq Expr where
    e1 == e2 = show e1 == show e2

instance Show Expr where
    show = foldExpr show showBinOp showUnaryOp
      where
        showBinOp e1 o e2 = "(" ++ e1 ++ " " ++ show o ++ " " ++ e2 ++ ")"
        showUnaryOp o e = "(" ++ show o ++ e ++ ")"
