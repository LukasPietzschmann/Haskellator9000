{-# LANGUAGE TemplateHaskell #-}

module Math.SiConverter.Internal.Operators(Op (..)) where

import Math.SiConverter.Internal.TH.OperGeneration (OperatorDef (..), generateOperators)

$(generateOperators [
    OperDef "Plus" "+",
    OperDef "Minus" "-",
    OperDef "Mult" "*",
    OperDef "Div" "/",
    OperDef "Pow" "^",
    OperDef "UnaryMinus" "-"
  ])
