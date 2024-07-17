{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Everything related to operators. See "Math.Haskellator.Internal.TH.OperGeneration" for what is available here.

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
