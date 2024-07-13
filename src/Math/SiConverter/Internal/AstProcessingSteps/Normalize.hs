module Math.SiConverter.Internal.AstProcessingSteps.Normalize (normalize) where

import Math.SiConverter.Internal.Expr (AstValue, Expr (..), Value (..), foldExpr)
import Math.SiConverter.Internal.Units (convertToBase)
import Math.SiConverter.Internal.Utils.Error (Error)

-- | Normalize all values inside the tree to their base units
normalize :: Expr              -- ^ the 'Expr' tree to normalize
          -> Either Error Expr -- ^ the normalized 'Expr' tree
normalize = Right . foldExpr (Val . convertDimensionToBase) BinOp UnaryOp Conversion VarBindings Var

convertDimensionToBase :: AstValue -> AstValue
convertDimensionToBase (Value v u) = foldr doIt (Value v []) u
    where doIt e (Value v' u') = let (Value v'' u'') = convertToBase $ Value 1 e
                                  in Value (v' * v'') (u'':u')
