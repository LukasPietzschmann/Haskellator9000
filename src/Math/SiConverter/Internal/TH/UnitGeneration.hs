{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Generate the unit types and function to work with them

module Math.SiConverter.Internal.TH.UnitGeneration(UnitDef(..), Quantity(..), generateUnits) where

import Language.Haskell.TH

-- | Definition of a Unit
data UnitDef = UnitDef String -- ^ Name of the unit
                       String -- ^ Abbreviation of the unit
                       Double -- ^ Factor to convert to the base unit

-- | A quantity made of a base unit and other related units
data Quantity = Quantity UnitDef   -- ^ Base unit
                         [UnitDef] -- ^ Other units

valueADT :: Name
valueADT = mkName "Value"

unitADT :: Name
unitADT = mkName "Unit"

unitFromStringFun :: Name
unitFromStringFun = mkName "unitFromString"

convertToBaseFun :: Name
convertToBaseFun = mkName "convertToBase"

-- | Generate the unit types and function to work with them. Imagine the following call: @generateUnits [Quantity (UnitDef "Meter" "m" 1) [UnitDef "Kilometer" "km" 1000]]@.
-- This function will then generate the following code:
--
--     * A data type with all the units
--
-- > data Unit = Meter | Kilometer
--
--     * An instance of Show for the data type
--
-- > instance Show Unit where
-- >   show Meter = "m"
-- >   show Kilometer = "km"
--
--     * A function to convert a string to a unit
--
-- > unitFromString :: String -> Either String Unit
-- > unitFromString "m" = Right Meter
-- > unitFromString "km" = Right Kilometer
-- > unitFromString x = Left x
--
--     * A function to convert a value to the base unit
--
-- > convertToBase :: Value -> Value
-- > convertToBase (Value v Meter) = Value (v * 1.0) Meter
-- > convertToBase (Value v KiloMeter) = Value (v * 1000.0) Meter
generateUnits :: [Quantity] -> Q [Dec]
generateUnits unitGroups = do
  let allUnits           = concatMap (\(Quantity b us) -> b:us) unitGroups
      unitConstructors   = (\(UnitDef name _ _) -> NormalC (mkName name) []) <$> allUnits
      showClauses        = mkShowClause <$> allUnits
      fromStringClauses  = (mkFromStringClause <$> allUnits) ++ [Clause [VarP $ mkName "x"] (NormalB $ AppE (ConE 'Left) (VarE $ mkName "x")) []]
      convertClauses     = concatMap (\(Quantity b us) -> mkConvertClaus b <$> b:us) unitGroups

      dataDec            = DataD [] unitADT [] Nothing unitConstructors [DerivClause Nothing []]
      showInstance       = InstanceD Nothing [] (AppT (ConT ''Show) (ConT unitADT)) [FunD 'show showClauses]
      fromStringSig      = SigD unitFromStringFun (AppT (AppT ArrowT (ConT ''String)) (AppT (AppT (ConT ''Either) (ConT ''String)) (ConT unitADT)))
      fromStringFunction = FunD unitFromStringFun fromStringClauses
      convertSig         = SigD convertToBaseFun (AppT (AppT ArrowT (ConT valueADT)) (ConT valueADT))
      convertFunction    = FunD convertToBaseFun convertClauses

  return [dataDec, showInstance, fromStringSig, fromStringFunction, convertSig, convertFunction]

mkShowClause :: UnitDef -> Clause
mkShowClause (UnitDef name abbrev _) =
  let pattern = ConP (mkName name) [] []
      body    = NormalB $ LitE $ StringL abbrev
  in Clause [pattern] body []

mkFromStringClause :: UnitDef -> Clause
mkFromStringClause (UnitDef name abbrev _) =
  let pattern = LitP $ StringL abbrev
      body    = NormalB $ AppE (ConE 'Right) (ConE $ mkName name)
  in Clause [pattern] body []

mkConvertClaus :: UnitDef -> UnitDef -> Clause
mkConvertClaus (UnitDef baseUnit _ _) (UnitDef unit _ factor) =
  let pattern = ConP valueADT [] [VarP (mkName "v"), ConP (mkName unit) [] []]
      body    = NormalB $ AppE (AppE (ConE valueADT) (InfixE (Just $ VarE $ mkName "v") (VarE $ mkName "*") (Just $ LitE $ RationalL $ toRational factor))) (ConE $ mkName baseUnit)
  in Clause [pattern] body []
