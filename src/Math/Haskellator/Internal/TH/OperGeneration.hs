{-# LANGUAGE TemplateHaskellQuotes #-}
module Math.Haskellator.Internal.TH.OperGeneration (
      OperatorDef (..)
    , generateOperators
    ) where

import Language.Haskell.TH

operADT :: Name
operADT = mkName "Op"

-- | Definition of a operator
data OperatorDef = OperDef String String

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

mkConstructor :: OperatorDef -> Con
mkConstructor (OperDef n _) = NormalC (mkName n) []

mkShowClause :: OperatorDef -> Clause
mkShowClause (OperDef n s) =
  let pattern = ConP (mkName n) [] []
      body    = NormalB $ LitE $ StringL s
  in Clause [pattern] body []
