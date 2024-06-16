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

data Expr = Value Double Unit | BinOp Expr Op Expr | UnaryOp Op Expr

foldExpr :: (Double -> Unit -> a) -> (a -> Op -> a -> a) -> (Op -> a -> a) -> Expr -> a
foldExpr fv fb fu = doIt
  where
    doIt (Value v u) = fv v u
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
-- example = BinOp (BinOp (Value 1 Multiplier) Plus (BinOp (Value 2 Multiplier) Mult (Value 3 Multiplier))) Minus (UnaryOp UnaryMinus (Value 4 Multiplier))
