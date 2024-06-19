{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Generate the unit types and function to work with them

module Math.SiConverter.Internal.TH.UnitGeneration(UnitDef(..), Quantity(..), generateUnits, OperatorDef(..), generateOperators) where

import Language.Haskell.TH

class Showable a where
    name :: a -> String
    abbreviation :: a -> String

-- | Definition of a Unit
data UnitDef = UnitDef String -- ^ Name of the unit
                       String -- ^ Abbreviation of the unit
                       Double -- ^ Factor to convert to the base unit

instance Showable UnitDef where
    name (UnitDef n _ _) = n
    abbreviation (UnitDef _ a _) = a

-- | A quantity made of a base unit and other related units
data Quantity = Quantity UnitDef   -- ^ Base unit
                         [UnitDef] -- ^ Other units

-- | Definition of a operator
data OperatorDef = OperDef String -- ^ Name of the operator
                           String -- ^ Mathematical symbol of the operator

instance Showable OperatorDef where
    name (OperDef n _) = n
    abbreviation (OperDef _ a) = a

valueADT :: Name
valueADT = mkName "Value"

unitADT :: Name
unitADT = mkName "Unit"

operADT :: Name
operADT = mkName "Op"

unitFromStringFun :: Name
unitFromStringFun = mkName "unitFromString"

convertToBaseFun :: Name
convertToBaseFun = mkName "convertToBase"

generateValueAdt :: [Dec]
generateValueAdt = [dataDec, showInstance]
  where dataDec = DataD [] valueADT [] Nothing [RecC valueADT [(mkName "value", Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Double), (mkName "unit", Bang NoSourceUnpackedness NoSourceStrictness, ConT unitADT)]] []
        showClauses = [Clause [ConP valueADT [] [VarP $ mkName "v", VarP $ mkName "u"]] (NormalB $ AppE (AppE (VarE '(++)) (AppE (VarE 'show) (VarE $ mkName "v"))) (AppE (VarE 'show) (VarE $ mkName "u"))) []]
        showInstance = InstanceD Nothing [] (AppT (ConT ''Show) (ConT valueADT)) [FunD 'show showClauses]

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
      unitConstructors   = mkConstructor <$> allUnits
      showClauses        = mkShowClause <$> allUnits
      fromStringClauses  = (mkFromStringClause <$> allUnits) ++ [Clause [VarP $ mkName "x"] (NormalB $ AppE (ConE 'Left) (VarE $ mkName "x")) []]
      convertClauses     = concatMap (\(Quantity b us) -> mkConvertClaus b <$> b:us) unitGroups

      dataDec            = DataD [] unitADT [] Nothing unitConstructors []
      showInstance       = InstanceD Nothing [] (AppT (ConT ''Show) (ConT unitADT)) [FunD 'show showClauses]
      fromStringSig      = SigD unitFromStringFun (AppT (AppT ArrowT (ConT ''String)) (AppT (AppT (ConT ''Either) (ConT ''String)) (ConT unitADT)))
      fromStringFunction = FunD unitFromStringFun fromStringClauses
      convertSig         = SigD convertToBaseFun (AppT (AppT ArrowT (ConT valueADT)) (ConT valueADT))
      convertFunction    = FunD convertToBaseFun convertClauses

  return $ [dataDec, showInstance] ++ generateValueAdt  ++ [fromStringSig, fromStringFunction, convertSig, convertFunction]

mkConvertClaus :: UnitDef -> UnitDef -> Clause
mkConvertClaus (UnitDef baseUnit _ _) (UnitDef unit _ factor) =
  let pattern = ConP valueADT [] [VarP (mkName "v"), ConP (mkName unit) [] []]
      body    = NormalB $ AppE (AppE (ConE valueADT) (InfixE (Just $ VarE $ mkName "v") (VarE $ mkName "*") (Just $ LitE $ RationalL $ toRational factor))) (ConE $ mkName baseUnit)
  in Clause [pattern] body []

-- | Generate the operator types and function to work with them. Imagine the following call: @generateOperators [OperDef "Plus" "+", OperDef "Minus" "-"]@.
-- This function will then generate the following code:
--
--     * A data type with all the operators
--
-- > data Op = Plus | Minus
--
--     * An instance of Show for the data type
--
-- > instance Show Op where
-- >   show Plus = "+"
-- >   show Minus = "-"
generateOperators :: [OperatorDef] -> Q [Dec]
generateOperators operators = do
  let operatorConstructors = mkConstructor <$> operators
      showClauses          = mkShowClause <$> operators

      dataDec              = DataD [] operADT [] Nothing operatorConstructors [DerivClause Nothing [ConT ''Enum, ConT ''Bounded]]
      showInstance         = InstanceD Nothing [] (AppT (ConT ''Show) (ConT operADT)) [FunD 'show showClauses]

  return [dataDec, showInstance]

mkConstructor :: Showable a => a -> Con
mkConstructor a = NormalC (mkName $ name a) []

mkShowClause :: Showable a => a -> Clause
mkShowClause a =
  let pattern = ConP (mkName $ name a) [] []
      body    = NormalB $ LitE $ StringL $ abbreviation a
  in Clause [pattern] body []

mkFromStringClause :: Showable a => a -> Clause
mkFromStringClause a =
  let pattern = LitP $ StringL $ abbreviation a
      body    = NormalB $ AppE (ConE 'Right) (ConE $ mkName $ name a)
  in Clause [pattern] body []
