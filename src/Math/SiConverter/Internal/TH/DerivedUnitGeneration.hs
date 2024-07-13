module Math.SiConverter.Internal.TH.DerivedUnitGeneration (
      DQuantity (..)
    , generateDerivedUnits
    ) where

import Language.Haskell.TH

import Math.SiConverter.Internal.TH.UnitGeneration (UnitDef)
import Math.SiConverter.Internal.Units (UnitExp)

data DQuantity = DQuantity UnitDef [UnitExp] [UnitDef]

-- TODO: Implement
generateDerivedUnits :: [DQuantity] -> Q [Dec]
generateDerivedUnits = return . const []
