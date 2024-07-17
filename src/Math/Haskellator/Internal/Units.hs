{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Everything related to units. See "Math.Haskellator.Internal.TH.UnitGeneration" for what is available here.

module Math.Haskellator.Internal.Units where

import Data.List (intercalate)

import Language.Haskell.TH.Syntax

import Math.Haskellator.Internal.TH.UnitGeneration

$(generateUnits
  [ Quantity (UnitDef "Multiplier" "" 1) [], -- Unitless unit
    Quantity (UnitDef "Meter" "m" 1) -- Length
    [ UnitDef "Kilometer" "km" 1000
    , UnitDef "Decimeter" "dm" 0.1
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
    [ UnitDef "Tonne" "t" 1000,
      UnitDef "Gram" "g" 1e-3
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

-- | An exponentiated unit
data UnitExp = UnitExp { dimUnit :: Unit
                       , power   :: Int
                       }
  deriving (Lift)

instance Show UnitExp where
    show (UnitExp u 1) = show u
    show (UnitExp u e) = show u ++ "^" ++ show e

instance Eq UnitExp where
    (UnitExp Multiplier _) == (UnitExp Multiplier _) = True
    (UnitExp u1 e1) == (UnitExp u2 e2)               = u1 == u2 && e1 == e2

-- | A dimension is a list of exponentiated units
type Dimension = [UnitExp]

instance {-# OVERLAPPING #-} Eq Dimension where
    a == b = all (`elem` b) a && all (`elem` a) b

instance {-# OVERLAPPING #-} Show Dimension where
    show xs = write $ divide xs ([],[])
              where
                write (pos,[])  = factors pos
                write ([],[UnitExp u e]) = "1/" ++ show (UnitExp u (abs e))
                write ([],neg)  = "1/(" ++ factors (makePos neg) ++ ")"
                write (pos,[UnitExp u e]) = factors pos ++ "/" ++ show (UnitExp u (abs e))
                write (pos,neg) = factors pos ++ "/(" ++ factors (makePos neg) ++ ")"

                factors list    = intercalate "*" (show <$> list)
                makePos ((UnitExp u e):us) = UnitExp u (abs e) : makePos us
                makePos []                 = []

-- | Splits a list of dimensions into units with positive and units negative exponents
divide::Dimension -> (Dimension,Dimension) -> (Dimension, Dimension)
divide (uExp@(UnitExp _ e):xs) (pos,neg) = if e<0 then divide xs (pos,neg ++ [uExp]) else divide xs (pos ++ [uExp], neg)
divide [] (pos,neg) = (pos,neg)
