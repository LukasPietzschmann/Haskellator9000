{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Generate the unit types and function to work with them

module Math.SiConverter.Internal.TH.UnitGeneration (
      OperatorDef (..)
    , Quantity (..)
    , UnitDef (..)
    , Value (..)
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

data Value u = Value { value :: Double
                     , unit  :: u
                     }

instance Show u => Show (Value u) where
      show (Value v u) = (++) (show v) (show u)

unitADT :: Name
unitADT = mkName "Unit"

simpleValue :: Type
simpleValue = AppT (ConT ''Value) (ConT unitExpADT)

unitExpADT :: Name
unitExpADT = mkName "UnitExp"


operADT :: Name
operADT = mkName "Op"

unitFromStringFun :: Name
unitFromStringFun = mkName "unitFromString"

convertToBaseFun :: Name
convertToBaseFun = mkName "convertToBase"

convertToFun :: Name
convertToFun = mkName "convertTo"

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
      unitConstructors   = mkConstructor <$> allUnits
      showClauses        = mkShowClause <$> allUnits
      fromStringClauses  = (mkFromStringClause <$> allUnits) -- RightCases
        ++ [Clause [VarP $ mkName "x"] (NormalB $ AppE (ConE 'Left) (VarE $ mkName "x")) []] -- Left case
      convertBaseClauses = concatMap (\(Quantity b us) -> mkConvertBaseClaus b <$> b:us) unitGroups
      convertToClauses   = concatMap (\(Quantity b us) -> mkConvertToClaus b <$> b:us) unitGroups

      dataDec            = DataD [] unitADT [] Nothing unitConstructors [DerivClause Nothing [ConT ''Eq]]
      showInstance       = InstanceD Nothing [] (AppT (ConT ''Show) (ConT unitADT)) [FunD 'show showClauses]
      fromStringSig      = SigD unitFromStringFun (AppT (AppT ArrowT (ConT ''String)) (AppT (AppT (ConT ''Either) (ConT ''String)) (ConT unitADT)))
      fromStringFunction = FunD unitFromStringFun fromStringClauses
      convertBaseSig     = SigD convertToBaseFun (AppT (AppT ArrowT simpleValue) simpleValue)
      convertBaseFunc    = FunD convertToBaseFun convertBaseClauses
      convertToSig       = SigD convertToFun (AppT (AppT ArrowT simpleValue) (AppT (AppT ArrowT (ConT unitADT)) simpleValue))
      convertToFunc      = FunD convertToFun convertToClauses
      isUnitFuns         = generateIsUnitFuns unitGroups

  return $ [dataDec, showInstance] ++ [fromStringSig, fromStringFunction, convertBaseSig, convertBaseFunc, convertToSig, convertToFunc] ++ isUnitFuns ++ genUnitExp

genUnitExp :: [Dec]
genUnitExp = [dataDec, showInstance, eqInstance]
  where dataDec = DataD [] unitExpADT [] Nothing [RecC unitExpADT [(mkName "dimUnit", Bang NoSourceUnpackedness NoSourceStrictness, ConT unitADT),
                                                                   (mkName "power", Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Int) ]] []
        showClauses = [Clause [ConP unitExpADT [] [VarP $ mkName "u", LitP $ IntegerL 1]]
          (NormalB $ AppE (VarE 'show) (VarE $ mkName "u")) [],
                      Clause [ConP unitExpADT [] [VarP $ mkName "u", VarP $ mkName "i"]]
          (NormalB $ AppE (AppE (VarE '(++)) (AppE (VarE 'show) (VarE $ mkName "u"))) (AppE (AppE (VarE '(++)) (LitE $ StringL "^")) (AppE (VarE 'show) (VarE $ mkName "i")))) []]
        showInstance = InstanceD Nothing [] (AppT (ConT ''Show) (ConT unitExpADT)) [FunD 'show showClauses]
        eqClauses = [Clause [ConP unitExpADT [] [VarP $ mkName "u1", VarP $ mkName "i1"],
                            ConP unitExpADT [] [VarP $ mkName "u2", VarP $ mkName "i2"]]
                      (NormalB $ InfixE
                      (Just $ InfixE (Just $ VarE $ mkName "u1") (VarE '(==)) (Just $ VarE $ mkName "u2"))
                      (VarE '(&&))
                      (Just $ InfixE (Just $ VarE $ mkName "i1") (VarE '(==)) (Just $ VarE $ mkName "i2"))
                      ) []]
        eqInstance   = InstanceD Nothing [] (AppT (ConT ''Eq) (ConT unitExpADT)) [FunD '(==) eqClauses]

generateIsUnitFuns :: [Quantity] -> [Dec]
generateIsUnitFuns unitGroups = concatMap mkIsUnitFun $ concatMap (\(Quantity b us) -> b:us) unitGroups

mkIsUnitFun :: UnitDef -> [Dec]
mkIsUnitFun (UnitDef unit _ _) = [SigD funName $ AppT (AppT ArrowT $ ConT unitADT) (ConT ''Bool), FunD funName [def, def']]
  where funName = mkName $ "is" ++ unit
        pattern = ConP (mkName unit) [] []
        body    = NormalB $ ConE 'True
        def     = Clause [pattern] body []
        body'   = NormalB $ ConE 'False
        def'    = Clause [WildP] body' []


mkConvertBaseClaus :: UnitDef -> UnitDef -> Clause
mkConvertBaseClaus (UnitDef baseUnit _ _) (UnitDef unit _ factor) =
  let pattern = ConP 'Value [] [VarP (mkName "v"), ConP unitExpADT [] [ConP (mkName unit) [] [], VarP (mkName "e")]]
      body    = NormalB $ AppE
        (AppE (ConE 'Value)
          (InfixE
            (Just $ VarE $ mkName "v")
            (VarE '(*))
            (Just $ InfixE
              (Just $ LitE $ RationalL $ toRational factor)
              (VarE '(^))
              (Just $ VarE $ mkName "e")
            )
          )
        )
        (AppE (AppE (ConE unitExpADT) (ConE $ mkName baseUnit)) (VarE $ mkName "e"))
  in Clause [pattern] body []

mkConvertToClaus :: UnitDef -> UnitDef -> Clause
mkConvertToClaus (UnitDef baseUnit _ _) (UnitDef unit _ factor) =
  let patVal = ConP 'Value [] [VarP (mkName "v"), ConP unitExpADT [] [ConP (mkName baseUnit) [] [], VarP (mkName "e")]]
      patUnit = ConP (mkName unit) [] []
      body    = NormalB $ AppE
        (AppE (ConE 'Value)
          (InfixE
            (Just $ VarE $ mkName "v")
            (VarE '(/))
            (Just $ InfixE
              (Just $ LitE $ RationalL $ toRational factor)
              (VarE '(^))
              (Just $ VarE $ mkName "e")
            )
          )
        )
        (AppE (AppE (ConE unitExpADT) (ConE $ mkName unit)) (VarE $ mkName "e"))
  in Clause [patVal, patUnit] body []

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
