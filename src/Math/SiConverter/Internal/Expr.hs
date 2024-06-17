{-# LANGUAGE TemplateHaskell #-}

-- | Models an expression tree
--
-- Examples:
--
-- >>> show $ BinOp (Val 1.0 Meter) Plus (BinOp (Val 2 Multiplier) Mult (Val 3.0 Meter))
-- "(1.0m + (2.0 * 3.0m))"
--
-- >>> show $ BinOp (BinOp (Val 1.0 Meter) Plus (Val 2.0 Meter)) Mult (Val 3.0 Multiplier)
-- "((1.0m + 2.0m) * 3.0)"

module Math.SiConverter.Internal.Expr(Expr(..),Op(..),Unit(..),foldExpr) where

import Math.SiConverter.Internal.TH.UnitGeneration(UnitDef(..), Quantity(..), generateUnits)

$(generateUnits [
    Quantity (UnitDef "Multiplier" "" 1) [],
    Quantity (UnitDef "Meter" "m" 1) [],
    Quantity (UnitDef "Second" "s" 1) [],
    Quantity (UnitDef "Kilo" "kg" 1) []
  ])

data Value = Value {
    value :: Double,
    unit  :: Unit
}

instance Show Value where
    show (Value v u) = show v ++ show u

data Op = Plus | Minus | Mult | Div | Pow | UnaryMinus
    deriving (Enum, Bounded)

instance Show Op where
  show Plus = "+"
  show Minus = "-"
  show Mult = "*"
  show Div = "/"
  show Pow = "^"
  show UnaryMinus = "-"

data Expr = Val Value | BinOp Expr Op Expr | UnaryOp Op Expr

-- | Folds an expression tree
foldExpr :: (Value -> a) -- ^ function that folds a value
         -> (a -> Op -> a -> a)   -- ^ function that folds a binary expression
         -> (Op -> a -> a)        -- ^ function that folds a unary expression
         -> Expr                  -- ^ the 'Expr' to fold over
         -> a                     -- ^ the resulting value
foldExpr fv fb fu = doIt
  where
    doIt (Val v) = fv v
    doIt (BinOp e1 o e2) = fb (doIt e1) o (doIt e2)
    doIt (UnaryOp o e) = fu o $ doIt e

instance Eq Expr where
  e1 == e2 = show e1 == show e2

instance Show Expr where
  show = foldExpr show showBinOp showUnaryOp
    where
      showBinOp e1 o e2 = "(" ++ e1 ++ " " ++ show o ++ " " ++ e2 ++ ")"
      showUnaryOp o e = "(" ++ show o ++ e ++ ")"
