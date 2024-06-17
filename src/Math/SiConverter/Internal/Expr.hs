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

data Unit = Multiplier | Second | Meter | Kilo

instance Show Unit where
  show Multiplier = ""
  show Second = "s"
  show Meter = "m"
  show Kilo = "kg"

data Op = Plus | Minus | Mult | Div | Pow | UnaryMinus
    deriving (Enum, Bounded)

instance Show Op where
  show Plus = "+"
  show Minus = "-"
  show Mult = "*"
  show Div = "/"
  show Pow = "^"
  show UnaryMinus = "-"

data Expr = Val Double Unit | BinOp Expr Op Expr | UnaryOp Op Expr

-- | Folds an expression tree
foldExpr :: (Double -> Unit -> a) -- ^ function that folds a value
         -> (a -> Op -> a -> a)   -- ^ function that folds a binary expression
         -> (Op -> a -> a)        -- ^ function that folds a unary expression
         -> Expr                  -- ^ the 'Expr' to fold over
         -> a                     -- ^ the resulting value
foldExpr fv fb fu = doIt
  where
    doIt (Val v u) = fv v u
    doIt (BinOp e1 o e2) = fb (doIt e1) o (doIt e2)
    doIt (UnaryOp o e) = fu o $ doIt e

instance Eq Expr where
  e1 == e2 = show e1 == show e2

instance Show Expr where
  show = foldExpr showValue showBinOp showUnaryOp
    where
      showValue v u = show v ++ show u
      showBinOp e1 o e2 = "(" ++ e1 ++ " " ++ show o ++ " " ++ e2 ++ ")"
      showUnaryOp o e = "(" ++ show o ++ e ++ ")"

-- example :: Expr
-- example = BinOp (BinOp (Val 1 Multiplier) Plus (BinOp (Val 2 Multiplier) Mult (Val 3 Multiplier))) Minus (UnaryOp UnaryMinus (Val 4 Multiplier))
