module Math.Haskellator.Internal.Utils.Error (Error (Error), Kind (..)) where

-- | Error type. Every error has a kind and a message.
data Error = Error Kind String
  deriving (Eq)

instance Show Error where
    show (Error k m) = show k ++ "\n" ++ m

data Kind = ParseError          -- ^ Error while syntactic analysis
          | ScanError           -- ^ Error while lexical analysis
          | RuntimeError        -- ^ Error during execution of the expression
          | ImplementationError -- ^ Error in the implementation. This is the only error that should ideally not happen.
  deriving (Eq, Show)
