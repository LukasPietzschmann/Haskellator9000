module Math.SiConverter.Internal.AstProcessingSteps.Normalize (
      convertDimensionTo
    , convertDimensionToBase
    , normalize
    ) where

import Math.SiConverter.Internal.Expr (AstValue, Expr (..), Value (..), foldExpr)
import Math.SiConverter.Internal.Units (UnitExp, convertTo, convertToBase, dimUnit,
           power)
import Math.SiConverter.Internal.Utils.Error (Error)

-- | Normalize all values inside the tree to their base units
normalize :: Expr              -- ^ the 'Expr' tree to normalize
          -> Either Error Expr -- ^ the normalized 'Expr' tree
normalize = Right . foldExpr (Val . convertDimensionToBase) BinOp UnaryOp Conversion VarBindings Var

convertDimensionToBase :: AstValue -> AstValue
convertDimensionToBase (Value v u) = foldr doIt (Value v []) u
    where doIt e (Value v' u') = let (Value v'' u'') = convertToBase $ Value 1 e
                                  in Value (v' * v'') (u'':u')

-- TODO: units can be arranged in a different order in source and target dimension
--       convert [m, s] [h, km] should be possible
convertDimensionTo :: AstValue -> [UnitExp] -> AstValue
convertDimensionTo (Value v src) target = foldr doIt (Value v []) (zip src target)
    where doIt (usrc, utrg) (Value v' src') = let (Value v'' src'') = convertTo (Value 1 $ dimUnit usrc) (dimUnit utrg) (power utrg)
                                               in Value (v' * v'') (src'':src')
