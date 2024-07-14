{-# LANGUAGE TemplateHaskell #-}

module Math.SiConverter.Internal.DerivedUnits where

import Math.SiConverter.Internal.TH.DerivedUnitGeneration (DQuantity (..),
           generateDerivedUnits)
import Math.SiConverter.Internal.TH.UnitGeneration (UnitDef (..))
import Math.SiConverter.Internal.Units (second, meter, kilogram)

$(generateDerivedUnits
  [ DQuantity (UnitDef "Hertz" "Hz" 1) (second (-1))
    [ UnitDef "Kilohertz" "kHz" 1000,
      UnitDef "Megahertz" "MHz" 1e6,
      UnitDef "Gigahertz" "GHz" 1e9,
      UnitDef "Terahertz" "THz" 1e12
    ]
--    DQuantity (UnitDef "Newton" "N" 1) (kilogram 1 * meter 1 * second (-2))
--    [ UnitDef "Kilonewton" "kN" 1000,
--      UnitDef "Meganewton" "MN" 1e6,
--      UnitDef "Giganewton" "GN" 1e9,
--      UnitDef "Teranewton" "GN" 1e12
--    ],
--    DQuantity (UnitDef "Pascal" "Pa" 1) (kilogram 1 * meter (-1) * second (-2))
--    [ UnitDef "Hectopascal" "hPa" 100,
--      UnitDef "Kilopascal"  "kPa" 1000,
--      UnitDef "Megapascal"  "MPa" 1e6,
--      UnitDef "Gigapascal"  "GPa" 1e9
--      UnitDef "Terapascal"  "TPa" 1e12
--    ],
--    DQuantity (UnitDef "Joule" "J" 1) (kilogram 1 * meter 2 * second (-2))
--    [ UnitDef "Kilojoule" "kJ" 1000,
--      UnitDef "Megajoule" "MJ" 1e6,
--      UnitDef "Gigajoule" "GJ" 1e9,
--      UnitDef "Terajoule" "TJ" 1e12
--    ],
--    DQuantity (UnitDef "Watt" "W" 1) (kilogram 1 * meter 2 * second (-3))
--    [
--      UnitDef "Milliwatt" "mW" 0.001, -- Only fractional derived unit required according to the project description
--      UnitDef "Kilowatt"  "kW" 1000,
--      UnitDef "Megawatt"  "MW" 1e6,
--      UnitDef "Gigawatt"  "GW" 1e9,
--      UnitDef "Terawatt"  "TW" 1e12
--    ]
  ])
