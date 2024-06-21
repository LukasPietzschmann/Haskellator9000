module Math.SiConverter.Internal.Utils.Error (Error (Error), Kind (..)) where

data Error = Error Kind String
  deriving (Eq)

instance Show Error where
    show (Error k m) = show k ++ "\n" ++ m

data Kind = ParseError | ScanError | ImplementationError
  deriving (Eq, Show)
