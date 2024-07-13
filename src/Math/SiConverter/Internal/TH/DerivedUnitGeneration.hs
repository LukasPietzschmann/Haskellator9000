{-# LANGUAGE TemplateHaskellQuotes, TupleSections #-}

module Math.SiConverter.Internal.TH.DerivedUnitGeneration (
      DQuantity (..)
    , generateDerivedUnits
    ) where

import Language.Haskell.TH

import Math.SiConverter.Internal.TH.UnitGeneration (UnitDef (..))
import Math.SiConverter.Internal.Units (UnitExp (..))

data DQuantity = DQuantity UnitDef [UnitExp] [UnitDef]

dunitFromStringFun :: Name
dunitFromStringFun = mkName "derivedUnitFromString"

generateDerivedUnits :: [DQuantity] -> Q [Dec]
generateDerivedUnits dunitGroups = do
    let allUnits = concatMap (\(DQuantity base units derived) -> (units,) <$> derived) dunitGroups
        fromStringClauses = (mkFromStringClause  <$> allUnits) ++ [return $ Clause [VarP $ mkName "x"] (NormalB $ AppE (ConE 'Left) (VarE $ mkName "x")) []]
    fromStringSig     <- sigD dunitFromStringFun [t|String -> Either String [UnitExp]|]
    fromStringFun     <- funD dunitFromStringFun fromStringClauses
    return [fromStringSig, fromStringFun]


mkFromStringClause :: ([UnitExp], UnitDef) -> Q Clause
mkFromStringClause (d, UnitDef _ a _) = do
  let pattern = LitP $ StringL a
  body <- normalB $ appE (conE 'Right) [|d|]
  return $ Clause [pattern] body []
