module Math.SiConverter.Internal.AstProcessingSteps.Normalize (normalize) where

import Math.SiConverter.Internal.Expr (Expr (..), convertToBase, foldExpr)
import Math.SiConverter.Internal.Utils.Error (Error)

-- | Normalize all values inside the tree to their base units
normalize :: Expr              -- ^ the 'Expr' tree to normalize
          -> Either Error Expr -- ^ the normalized 'Expr' tree
normalize = Right . foldExpr (Val . convertToBase) BinOp UnaryOp VarBindings Var
