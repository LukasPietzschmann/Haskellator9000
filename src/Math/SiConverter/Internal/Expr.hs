{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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
    , UnitExp (..)
    , Value (..)
    , convertToBase
    , convertTo
    , foldExpr
    , foldExprM
    , isMultiplier
    , unitFromString
    ) where

import Math.SiConverter.Internal.TH.UnitGeneration (OperatorDef (..), Quantity (..),
           UnitDef (..), generateOperators, generateUnits, Value(..))

$(generateUnits 
  [ Quantity (UnitDef "Multiplier" "" 1) [], -- Unitless unit
    Quantity (UnitDef "Meter" "m" 1) -- Length
    [ UnitDef "Kilometer" "km" 1000
    , UnitDef "Centimeter" "cm" 0.01
    , UnitDef "Millimeter" "mm" 0.001
    , UnitDef "Micrometer" "µm" 1e-6
    , UnitDef "Nanometer" "nm" 1e-9
    --, UnitDef "Picometer" "pm" 1e-12
    --, UnitDef "Femtometer" "fm" 1e-15
    --, UnitDef "Attometer" "am" 1e-18
    --, UnitDef "Zeptometer" "zm" 1e-21
    --, UnitDef "Yoctometer" "ym" 1e-24
    ]
  , Quantity (UnitDef "Second" "s" 1) -- Time
    [ UnitDef "Minute" "min" 60
    , UnitDef "Hour" "h" 3600
    , UnitDef "Day" "d" 86400
    , UnitDef "Millisecond" "ms" 1e-3
    , UnitDef "Microsecond" "µs" 1e-6
    , UnitDef "Nanosecond" "ns" 1e-9
    --, UnitDef "Picosecond" "ps" 1e-12
    --, UnitDef "Femtosecond" "fs" 1e-15
    --, UnitDef "Attosecond" "as" 1e-18
    --, UnitDef "Zeptosecond" "zs" 1e-21
    --, UnitDef "Yoctosecond" "ys" 1e-24
    ]
  , Quantity (UnitDef "Kilogram" "kg" 1) -- Mass
    [ UnitDef "Gram" "g" 1e-3
    , UnitDef "Milligram" "mg" 1e-6
    , UnitDef "Microgram" "µg" 1e-9
    , UnitDef "Nanogram" "ng" 1e-12
    --, UnitDef "Picogram" "pg" 1e-15
    --, UnitDef "Femtogram" "fg" 1e-18
    --, UnitDef "Attogram" "ag" 1e-21
    --, UnitDef "Zeptogram" "zg" 1e-24
    --, UnitDef "Yoctogram" "yg" 1e-27
    ]
  ])

$(generateOperators [
    OperDef "Plus" "+",
    OperDef "Minus" "-",
    OperDef "Mult" "*",
    OperDef "Div" "/",
    OperDef "Pow" "^",
    OperDef "UnaryMinus" "-"
  ])

type AstValue = Value UnitExp

data Expr = Val AstValue
          | BinOp Expr Op Expr
          | UnaryOp Op Expr
          | Conversion Expr Unit

-- | Folds an expression tree
foldExpr :: (AstValue -> a)     -- ^ function that folds a value
         -> (a -> Op -> a -> a) -- ^ function that folds a binary expression
         -> (Op -> a -> a)      -- ^ function that folds a unary expression
         -> (a -> Unit -> a)    -- ^ function that folds a conversion expression
         -> Expr                -- ^ the 'Expr' to fold over
         -> a                   -- ^ the resulting value
foldExpr fv fb fu fc = doIt
  where
    doIt (Val v)         = fv v
    doIt (BinOp e1 o e2) = fb (doIt e1) o (doIt e2)
    doIt (UnaryOp o e)   = fu o $ doIt e
    doIt (Conversion e u) = fc (doIt e) u

-- | Monadic fold over an expression tree
foldExprM :: (Monad m) => (AstValue -> m a) -- ^ function that folds a value
         -> (a -> Op -> a -> m a)           -- ^ function that folds a binary expression
         -> (Op -> a -> m a)                -- ^ function that folds a unary expression
         -> (a -> Unit -> m a)              -- ^ function that folds a conversion expression
         -> Expr                            -- ^ the 'Expr' to fold over
         -> m a                             -- ^ the resulting value
foldExprM fv fb fu fc = doIt
  where
    doIt (Val v) = fv v
    doIt (BinOp e1 o e2) = do
        v1 <- doIt e1
        v2 <- doIt e2
        fb v1 o v2
    doIt (UnaryOp o e) = doIt e >>= \v -> fu o v
    doIt (Conversion e u) = doIt e >>= \v -> fc v u

instance Eq Expr where
    e1 == e2 = show e1 == show e2

instance Show Expr where
    show = foldExpr show showBinOp showUnaryOp showConversion
      where
        showBinOp e1 o e2  = "(" ++ e1 ++ " " ++ show o ++ " " ++ e2 ++ ")"
        showUnaryOp o e    = "(" ++ show o ++ e ++ ")"
        showConversion e u = e ++ "[" ++ show u ++ "]"
