{-# LANGUAGE TemplateHaskell #-}

module Math.SiConverter.Internal.DerivedUnits where

import Math.SiConverter.Internal.TH.DerivedUnitGeneration (DQuantity (..),
           generateDerivedUnits)
import Math.SiConverter.Internal.TH.UnitGeneration (UnitDef (..))
import Math.SiConverter.Internal.Units (second)

$(generateDerivedUnits
  [ DQuantity (UnitDef "Herz" "Hz" 1) (second (-1))
    [ UnitDef "Kilohertz" "kHz" 1000,
      UnitDef "Megahertz" "MHz" 1e6,
      UnitDef "Gigahertz" "GHz" 1e9,
      UnitDef "Terahertz" "THz" 1e12
    ]
  ])
