{-# LANGUAGE TemplateHaskell #-}

module Math.Haskellator.Internal.Operators where

import Math.Haskellator.Internal.TH.OperGeneration

$(generateOperators [
    OperDef "Plus" "+",
    OperDef "Minus" "-",
    OperDef "Mult" "*",
    OperDef "Div" "/",
    OperDef "Pow" "^",
    OperDef "UnaryMinus" "-"
  ])
