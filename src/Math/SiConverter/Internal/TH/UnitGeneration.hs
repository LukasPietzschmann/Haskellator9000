{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Generate the unit types and function to work with them

module Math.SiConverter.Internal.TH.UnitGeneration (
      OperatorDef (..)
    , Quantity (..)
    , UnitDef (..)
    , generateOperators
    , generateUnits
    ) where

import Language.Haskell.TH

class Showable a where
    name :: a -> String
    abbreviation :: a -> String

-- | Definition of a Unit
data UnitDef = UnitDef String String Double

instance Showable UnitDef where
    name (UnitDef n _ _) = n
    abbreviation (UnitDef _ a _) = a

-- | A quantity made of a base unit and other related units
data Quantity = Quantity UnitDef [UnitDef]

-- | Definition of a operator
data OperatorDef = OperDef String String

instance Showable OperatorDef where
    name (OperDef n _) = n
    abbreviation (OperDef _ a) = a

valueADT :: Name
valueADT = mkName "Value"

unitADT :: Name
unitADT = mkName "Unit"

simpleValue :: Type
simpleValue = AppT (ConT valueADT) (ConT unitADT)

operADT :: Name
operADT = mkName "Op"

unitFromStringFun :: Name
unitFromStringFun = mkName "unitFromString"

convertToBaseFun :: Name
convertToBaseFun = mkName "convertToBase"

generateValueAdt :: [Dec]
generateValueAdt = [dataDec, showInstance]
  where dataDec = DataD [] valueADT [PlainTV (mkName "u") ()] Nothing [RecC valueADT [(mkName "value", Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Double), (mkName "unit", Bang NoSourceUnpackedness NoSourceStrictness, VarT $ mkName "u")]] []
        showClauses = [Clause [ConP valueADT [] [VarP $ mkName "v", VarP $ mkName "u"]] (NormalB $ AppE (AppE (VarE '(++)) (AppE (VarE 'show) (VarE $ mkName "v"))) (AppE (VarE 'show) (VarE $ mkName "u"))) []]
        showInstance = InstanceD Nothing [AppT (ConT ''Show) (VarT $ mkName "u")] (AppT (ConT ''Show) (AppT (ConT valueADT) (VarT $ mkName "u"))) [FunD 'show showClauses]

-- | Generate the unit types and function to work with them. Imagine the following call: @generateUnits [Quantity (UnitDef "Meter" "m" 1) [UnitDef "Kilometer" "km" 1000]]@.
-- This function will then generate the following code:
--
--     * A data type with all the units
--
-- > data Unit = Meter Int | Kilometer Int
--
--     * An instance of Show for the data type
--
-- > instance Show Unit where -- e is the exponent (is not equal to 1)
-- >   show Meter = "m^e"
-- >   show Kilometer = "km^e"
--
--     * A function to convert a string to a unit
--
-- > unitFromString :: String -> Either String Unit
-- > unitFromString "m" = Right (Meter 1)
-- > unitFromString "km" = Right (Kilometer 1)
-- > unitFromString x = Left x
--
--     * A function to convert a value to the base unit
--
-- > convertToBase :: Value -> Value
-- > convertToBase (Value v (Meter e)) = Value ((v * 1.0) ^ e) (Meter e)
-- > convertToBase (Value v (KiloMeter e)) = Value ((v * 0.0001) ^ e) (Meter e)
--
--     * A function to check whether a given unit is a specific unit
--
-- > isMeter :: Unit -> Bool
-- > isMeter (Meter _) = True
-- > isMeter _ = False
-- > isKilometer :: Unit -> Bool
-- > isKilometer (Kilometer _) = True
-- > isKilometer _ = False
generateUnits :: [Quantity] -> Q [Dec]
generateUnits unitGroups = do
  let allUnits           = concatMap (\(Quantity b us) -> b:us) unitGroups
      unitConstructors   = mkConstructorWithInt <$> allUnits
      showClauses        = mkShowClauseWithInt <$> allUnits
      fromStringClauses  = (mkFromStringClause <$> allUnits) -- RightCases
        ++ [Clause [VarP $ mkName "x"] (NormalB $ AppE (ConE 'Left) (VarE $ mkName "x")) []] -- Left case
      convertClauses     = concatMap (\(Quantity b us) -> mkConvertClaus b <$> b:us) unitGroups

      dataDec            = DataD [] unitADT [] Nothing unitConstructors [DerivClause Nothing [ConT ''Eq]]
      showInstance       = InstanceD Nothing [] (AppT (ConT ''Show) (ConT unitADT)) [FunD 'show showClauses]
      fromStringSig      = SigD unitFromStringFun (AppT (AppT ArrowT (ConT ''String)) (AppT (AppT (ConT ''Either) (ConT ''String)) (ConT unitADT)))
      fromStringFunction = FunD unitFromStringFun fromStringClauses
      convertSig         = SigD convertToBaseFun (AppT (AppT ArrowT simpleValue) simpleValue)
      convertFunction    = FunD convertToBaseFun convertClauses
      isUnitFuns         = generateIsUnitFuns unitGroups

  return $ [dataDec, showInstance] ++ generateValueAdt  ++ [fromStringSig, fromStringFunction, convertSig, convertFunction] ++ isUnitFuns

generateIsUnitFuns :: [Quantity] -> [Dec]
generateIsUnitFuns unitGroups = mkIsUnitFun <$> concatMap (\(Quantity b us) -> b:us) unitGroups

mkIsUnitFun :: UnitDef -> Dec
mkIsUnitFun (UnitDef unit _ _) = FunD funName [def, def']
  where funName = mkName $ "is" ++ unit
        pattern = ConP (mkName unit) [] [WildP]
        body    = NormalB $ ConE 'True
        def     = Clause [pattern] body []
        body'   = NormalB $ ConE 'False
        def'    = Clause [WildP] body' []

-- TODO Consider exponents (e.g. 1km^2 = 1 000 000 m^2 != 1000 m^2)!
mkConvertClaus :: UnitDef -> UnitDef -> Clause
mkConvertClaus (UnitDef baseUnit _ _) (UnitDef unit _ factor) =
  let pattern = ConP valueADT [] [VarP (mkName "v"), ConP (mkName unit) [] [VarP (mkName "e")]]
      body    = NormalB $ AppE (AppE (ConE valueADT) (InfixE (Just $ InfixE (Just $ VarE $ mkName "v") (VarE '(*)) (Just $ LitE $ RationalL $ toRational factor)) (VarE '(^)) (Just $ VarE $ mkName "e"))) (AppE (ConE $ mkName baseUnit) (VarE $ mkName "e"))
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

mkConstructorWithInt :: Showable a => a -> Con
mkConstructorWithInt a = NormalC (mkName $ name a) [(Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Int)]

mkShowClause :: Showable a => a -> Clause
mkShowClause a =
  let pattern = ConP (mkName $ name a) [] []
      body    = NormalB $ LitE $ StringL $ abbreviation a
  in Clause [pattern] body []

mkShowClauseWithInt :: Showable a => a -> Clause
mkShowClauseWithInt a =
  let pattern = ConP (mkName $ name a) [] [WildP]
      body    = NormalB $ LitE $ StringL $ abbreviation a
  in Clause [pattern] body []

mkFromStringClause :: Showable a => a -> Clause
mkFromStringClause a =
  let pattern = LitP $ StringL $ abbreviation a
      body    = NormalB $ AppE (ConE 'Right) (AppE (ConE $ mkName $ name a) (LitE $ IntegerL 1))
  in Clause [pattern] body []
