{-# LANGUAGE TemplateHaskellQuotes, TupleSections #-}

-- | Generate the unit types and function to work with them

module Math.Haskellator.Internal.TH.DerivedUnitGeneration (
      DQuantity (..)
    , generateDerivedUnits
    ) where

import Language.Haskell.TH

import Math.Haskellator.Internal.TH.UnitGeneration
import Math.Haskellator.Internal.Units

-- | A derived quantity. Works in the same way as 'Quantity', but with an additional
-- 'Dimension' representing it.
data DQuantity = DQuantity UnitDef Dimension [UnitDef]

dunitFromStringFun :: Name
dunitFromStringFun = mkName "derivedUnitFromString"

-- | Since derived units are instantly converted to the 'Dimension' representing them,
-- this function will only generate one additional function 'derivedUnitFromString'.
-- Imagine the call @generateDerivedUnits [DQuantity (UnitDef "Herz" "Hz" 1) [UnitExp Second (-1)] [] []@.
--
-- > derivedUnitFromString :: String -> Either String Dimension
-- > derivedUnitFromString "Hz" = Right [UnitExp Second (-1)]
-- > derivedUnitFromString x = Left x
generateDerivedUnits :: [DQuantity] -> Q [Dec]
generateDerivedUnits dunitGroups = do
    let allUnits = concatMap (\(DQuantity base units derived) -> (units,) <$> base:derived) dunitGroups
        fromStringClauses = (mkFromStringClause <$> allUnits) ++ [return $ Clause [VarP $ mkName "x"] (NormalB $ AppE (ConE 'Left) (VarE $ mkName "x")) []]
    fromStringSig     <- sigD dunitFromStringFun [t|String -> Either String Dimension|]
    fromStringFun     <- funD dunitFromStringFun fromStringClauses
    return [fromStringSig, fromStringFun]


mkFromStringClause :: (Dimension, UnitDef) -> Q Clause
mkFromStringClause (d, UnitDef _ a _) = do
  let pattern = LitP $ StringL a
  body <- normalB $ appE (conE 'Right) [|d|]
  return $ Clause [pattern] body []
