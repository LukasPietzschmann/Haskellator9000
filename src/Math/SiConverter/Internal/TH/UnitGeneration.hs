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

import Data.Char (toLower)

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
-- > convertToBase :: Value UnitExp -> Value UnitExp
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
--
--     * A function that creates a UnitExp
--
-- > meter :: Int -> UnitExp
-- > meter e = UnitExp $ Meter e
-- > kilometers :: Int -> UnitExp
-- > kilometers e = UnitExp $ Kilometer e
generateUnits :: [Quantity] -> Q [Dec]
generateUnits unitGroups = do
  let allUnits           = concatMap (\(Quantity b us) -> b:us) unitGroups
      unitConstructors   = mkConstructor <$> allUnits
      showClauses        = mkShowClause <$> allUnits
      fromStringClauses  = (mkFromStringClause <$> allUnits) -- RightCases
        ++ [Clause [VarP $ mkName "x"] (NormalB $ AppE (ConE 'Left) (VarE $ mkName "x")) []] -- Left case
      convertBaseClauses = concatMap (\(Quantity b us) -> mkConvertBaseClaus b <$> b:us) unitGroups
      convertToClauses   = concatMap (\(Quantity b us) -> mkConvertToClaus b <$> b:us) unitGroups

      dataDec            = DataD [] unitADT [] Nothing unitConstructors [DerivClause Nothing [ConT ''Eq, ConT ''Bounded, ConT ''Enum]]
      showInstance       = InstanceD Nothing [] (AppT (ConT ''Show) (ConT unitADT)) [FunD 'show showClauses]
      fromStringFunction = FunD unitFromStringFun fromStringClauses

  fromStringSig   <- sigD unitFromStringFun [t|String -> Either String $(conT unitADT)|]
  convertToSig    <- sigD convertToFun [t|Value $(conT unitADT) -> $(conT unitADT) -> Int -> Value $(conT unitExpADT)|]
  convertToFunc   <- funD convertToFun convertToClauses
  convertBaseSig  <- sigD convertToBaseFun [t|Value $(conT unitExpADT) -> Value $(conT unitExpADT)|]
  convertBaseFunc <- funD convertToBaseFun convertBaseClauses
  mkUnitFuns      <- generateMkUnitFuns unitGroups
  isUnitFuns      <- generateIsUnitFuns unitGroups
  unitExpDec      <- genUnitExp

  return $ [dataDec, showInstance] ++ [fromStringSig, fromStringFunction, convertBaseSig, convertBaseFunc, convertToSig, convertToFunc] ++ isUnitFuns ++ unitExpDec ++ mkUnitFuns

genUnitExp :: Q [Dec]
genUnitExp = [d|
  data UnitExp = UnitExp { dimUnit :: $(conT unitADT), power :: Int }

  instance Show UnitExp where
    show (UnitExp u 1) = show u
    show (UnitExp u i) = show u ++ "^" ++ show i

  instance Eq UnitExp where
    (UnitExp u1 i1) == (UnitExp u2 i2) = u1 == u2 && i1 == i2
  |]

generateIsUnitFuns :: [Quantity] -> Q [Dec]
generateIsUnitFuns unitGroups = concat <$> mapM mkIsUnitFun (concatMap (\(Quantity b us) -> b:us) unitGroups)

mkIsUnitFun :: UnitDef -> Q [Dec]
mkIsUnitFun (UnitDef u _ _) = do
    let funName = mkName $ "is" ++ u
        pattern = ConP (mkName u) [] []
        body    = NormalB $ ConE 'True
        def     = Clause [pattern] body []
        body'   = NormalB $ ConE 'False
        def'    = Clause [WildP] body' []
    sig <- sigD funName [t|$(conT unitADT) -> Bool|]
    return [sig, FunD funName [def, def']]

generateMkUnitFuns :: [Quantity] -> Q [Dec]
generateMkUnitFuns unitGroups = concat <$> mapM mkMkUnitFun (concatMap (\(Quantity b us) -> b:us) unitGroups)

mkMkUnitFun :: UnitDef -> Q [Dec]
mkMkUnitFun (UnitDef u _ _) = do
    let funName = mkName $ toLower <$> u
        pattern = VarP $ mkName "e"
    body <- normalB [|UnitExp $(conE $ mkName u) e|]
    return [SigD funName $ AppT (AppT ArrowT $ ConT ''Int) (ConT unitExpADT), FunD funName [Clause [pattern] body []]]

mkConvertBaseClaus :: UnitDef -> UnitDef -> Q Clause
mkConvertBaseClaus (UnitDef baseUnit _ _) (UnitDef u _ f) = do
    let pattern = ConP 'Value [] [VarP (mkName "v"), ConP unitExpADT [] [ConP (mkName u) [] [], VarP (mkName "e")]]
    body <- normalB [|Value (v * ($(litE $ RationalL $ toRational f) ^ e)) (UnitExp $(conE $ mkName baseUnit) e)|]
    return $ Clause [pattern] body []

mkConvertToClaus :: UnitDef -> UnitDef -> Q Clause
mkConvertToClaus (UnitDef baseUnit _ _) (UnitDef u _ f) = do
    let patVal  = ConP 'Value [] [VarP (mkName "v"), ConP (mkName baseUnit) [] []]
        patUnit = ConP (mkName u) [] []
        patExp  = VarP $ mkName "e"
    body <- normalB [|Value (v / ($(litE $ RationalL $ toRational f) ^ e)) (UnitExp $(conE $ mkName u) e)|]
    return $ Clause [patVal, patUnit, patExp] body []

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
