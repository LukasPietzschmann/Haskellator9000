module Math.SiConverter.Internal.AstProcessingSteps.Normalize (normalize) where

import Math.SiConverter.Internal.Expr (Expr (..), foldExpr)
import Math.SiConverter.Internal.Units (convertToBase)
import Math.SiConverter.Internal.Utils.Error (Error)

-- | Normalize all values inside the tree to their base units
normalize :: Expr              -- ^ the 'Expr' tree to normalize
          -> Either Error Expr -- ^ the normalized 'Expr' tree
normalize = Right . foldExpr (Val . convertToBase) BinOp UnaryOp Conversion VarBindings Var
