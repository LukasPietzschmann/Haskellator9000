module Math.SiConverter.Internal.AstProcessingSteps.Normalize (
      convertDimensionTo
    , convertDimensionToBase
    , normalize
    , tryConvertDimensionTo
    ) where

import Data.Maybe (fromJust)

import Math.SiConverter.Internal.Expr (AstValue, Expr (..), Value (..), foldExpr)
import Math.SiConverter.Internal.Units (Dimension, UnitExp, convertTo, convertToBase,
           dimUnit, power)
import Math.SiConverter.Internal.Utils.Composition ((.:))
import Math.SiConverter.Internal.Utils.Error (Error)

-- | Normalize all values inside the tree to their base units
normalize :: Expr              -- ^ the 'Expr' tree to normalize
          -> Either Error Expr -- ^ the normalized 'Expr' tree
normalize = Right . foldExpr (Val . convertDimensionToBase) BinOp UnaryOp Conversion VarBindings Var

convertDimensionToBase :: AstValue -> AstValue
convertDimensionToBase (Value v u) = foldr doIt (Value v []) u
    where doIt e (Value v' u') = let (Value v'' u'') = convertToBase $ Value 1 e
                                  in Value (v' * v'') (u'':u')

-- | Converts a value to a given dimension. Requites that the conversion is possible.
convertDimensionTo :: AstValue  -- ^ the value to convert
                   -> Dimension -- ^ the target dimension
                   -> AstValue  -- ^ the converted value
convertDimensionTo = fromJust .: tryConvertDimensionTo

-- | Tries to convert a value to a given dimension. Returns 'Nothing' if the conversion is not possible.
tryConvertDimensionTo :: AstValue       -- ^ the value to convert
                      -> Dimension      -- ^ the target dimension
                      -> Maybe AstValue -- ^ the converted value or 'Nothing'
tryConvertDimensionTo (Value v src) target = convertDimensions src target $ Value v []

convertDimensions :: Dimension      -- ^ Source dimension
                  -> Dimension      -- ^ Target dimension
                  -> AstValue       -- ^ Value to convert (Should only contain the numeric value but an empty dimension)
                  -> Maybe AstValue -- ^ Converted value (will contain the correct dimension) or 'Nothing'
convertDimensions [] [] a = Just a
convertDimensions [] _  _ = Nothing
convertDimensions (s:ss) ts v = case convertUnit s ts v of
    Just (v', rest) -> convertDimensions ss rest v'
    Nothing         -> Nothing

-- | Converts a unit to a mathing unit in the target dimension
convertUnit :: UnitExp                     -- ^ Source unit
            -> Dimension                   -- ^ Target dimension
            -> AstValue                    -- ^ Value to convert
            -> Maybe (AstValue, Dimension) -- ^ Converted value and the remaining target dimension or 'Nothing'
convertUnit _ [] _ = Nothing
convertUnit s (t:ts) val@(Value v u) = case convertTo (Value 1 $ dimUnit s) (dimUnit t) (power s) of
    Just (Value v' u') -> Just (Value (v * v') (u':u), ts)
    Nothing            -> do
        (v', rest) <- convertUnit s ts val
        return (v', t:rest)
