{-# LANGUAGE TemplateHaskellQuotes, TupleSections #-}

module Math.Haskellator.Internal.TH.DerivedUnitGeneration (
      DQuantity (..)
    , generateDerivedUnits
    ) where

import Language.Haskell.TH

import Math.Haskellator.Internal.TH.UnitGeneration
import Math.Haskellator.Internal.Units

data DQuantity = DQuantity UnitDef Dimension [UnitDef]

dunitFromStringFun :: Name
dunitFromStringFun = mkName "derivedUnitFromString"

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
