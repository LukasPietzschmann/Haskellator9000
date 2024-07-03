{-# LANGUAGE TemplateHaskell #-}

-- | Models an expression tree
--
-- Examples:
--
-- >>> show $ BinOp (Val $ Value 1.0 $ Meter 1) Plus (BinOp (Val $ Value 2 $ Multiplier 1) Mult (Val $ Value 3.0 $ Meter 1))
-- "(1.0m + (2.0 * 3.0m))"
--
-- >>> show $ BinOp (BinOp (Val $ Value 1.0 $ Meter 1) Plus (Val $ Value 2.0 $ Meter 1)) Mult (Val $ Value 3.0 $ Multiplier 1)
-- "((1.0m + 2.0m) * 3.0)"

module Math.SiConverter.Internal.Expr (
      AstFold
    , AstValue
    , Bindings
    , Expr (..)
    , Op (..)
    , SimpleAstFold
    , Thunk (..)
    , Unit (..)
    , Value (..)
    , bindVar
    , bindVars
    , convertToBase
    , foldExpr
    , getVarBinding
    , isMultiplier
    , partiallyFoldExprM
    , runAstFold
    , runInNewScope
    , unitFromString
    ) where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State (State, evalState, get, modify)

import Data.List (intercalate)
import Data.Map (Map, insert, (!?))

import Math.SiConverter.Internal.TH.UnitGeneration (OperatorDef (..), Quantity (..),
           UnitDef (..), generateOperators, generateUnits)
import Math.SiConverter.Internal.Utils.Composition ((.:))
import Math.SiConverter.Internal.Utils.Error (Error (Error), Kind (..))
import Math.SiConverter.Internal.Utils.Stack (Stack, mapTop, pop, push, top)

$(generateUnits [
    Quantity (UnitDef "Multiplier" "" 1) [],
    Quantity (UnitDef "Meter" "m" 1) [],
    Quantity (UnitDef "Second" "s" 1) [],
    Quantity (UnitDef "Kilo" "kg" 1) []
  ])

$(generateOperators [
    OperDef "Plus" "+",
    OperDef "Minus" "-",
    OperDef "Mult" "*",
    OperDef "Div" "/",
    OperDef "Pow" "^",
    OperDef "UnaryMinus" "-"
  ])

type AstValue = Value Unit

-- | A list of variable bindings, mapping a name to an arbitrary value
type Bindings a = [(String, a)]

data Expr = Val AstValue
          | BinOp Expr Op Expr
          | UnaryOp Op Expr
          | VarBindings (Bindings Expr) Expr
          | Var String

data Thunk a = Expr Expr
             | Result a

-- | Folds an expression tree
foldExpr :: (AstValue -> a)        -- ^ function that folds a value
         -> (a -> Op -> a -> a)    -- ^ function that folds a binary expression
         -> (Op -> a -> a)         -- ^ function that folds a unary expression
         -> (Bindings a -> a -> a) -- ^ function that folds variable bindings
         -> (String -> a)          -- ^ function that folds a variable
         -> Expr                   -- ^ the 'Expr' to fold over
         -> a                      -- ^ the resulting value
foldExpr fv fb fu fvb fvn = doIt
  where
    doIt (Val v)            = fv v
    doIt (BinOp e1 o e2)    = fb (doIt e1) o (doIt e2)
    doIt (UnaryOp o e)      = fu o $ doIt e
    doIt (VarBindings bs e) = fvb (fmap doIt <$> bs) (doIt e)
    doIt (Var n)            = fvn n

-- | Encapsulates the result 'b' of folding an expression tree and holds the current
-- state of variable bindings of type 'a'
type AstFold a b = ExceptT Error (State (Stack (Map String (Thunk a)))) b

-- | Simplified version of 'AstFold' that returns the same type as it binds to variables
type SimpleAstFold a = AstFold a a

-- | Retrieves the value bound to a variable name
getVarBinding :: String              -- ^ the variable name
              -> AstFold a (Thunk a) -- ^ the 'Thunk' bound to the variable
getVarBinding n = do
    context <- get
    case top context !? n of
        Just v -> return v
        Nothing -> throwError $ Error RuntimeError $ "Variable '" ++ n ++ "' not in scope"

-- | Binds a 'Thunk' to a variable name
bindVar :: String       -- ^ the variable name
        -> Thunk a      -- ^ the value to bind
        -> AstFold a ()
bindVar = modify . mapTop .: insert

-- | Binds multiple variable names
bindVars :: Bindings (Thunk a) -- ^ the variable names and their values
         -> AstFold a ()
bindVars = mapM_ $ uncurry bindVar

-- | Evaluates an 'SimpleAstFold' inside a new scope
runInNewScope :: SimpleAstFold a -- ^ the computation to run
              -> SimpleAstFold a -- ^ the computation's result
runInNewScope f = do
    modify $ push mempty
    result <- f
    modify $ snd . pop
    return result

-- | Runs an 'SimpleAstFold' computation
runAstFold :: SimpleAstFold a      -- ^ the computation to run
           -> Either Error a -- ^ the result of the computation
runAstFold = flip evalState (push mempty mempty) . runExceptT

-- | Like 'foldExpr', but does not fold into variable bindings and returns a monadic
-- result
partiallyFoldExprM :: (AstValue -> SimpleAstFold a) -> (a -> Op -> a -> SimpleAstFold a) -> (Op -> a -> SimpleAstFold a) -> (Bindings Expr -> Expr -> SimpleAstFold a) -> (String -> SimpleAstFold a) -> Expr -> SimpleAstFold a
partiallyFoldExprM fv fb fu fbv fvar = doIt
    where
        doIt (Val v) = fv v
        doIt (BinOp lhs op rhs) = do
            l <- doIt lhs
            r <- doIt rhs
            fb l op r
        doIt (UnaryOp op e) = do
            v <- doIt e
            fu op v
        doIt (VarBindings bs expr) = fbv bs expr
        doIt (Var n) = fvar n

instance Eq Expr where
    e1 == e2 = show e1 == show e2

instance Show Expr where
    show = foldExpr show showBinOp showUnaryOp showVarBinds id
      where
        showBinOp e1 o e2 = "(" ++ e1 ++ " " ++ show o ++ " " ++ e2 ++ ")"
        showUnaryOp o e = "(" ++ show o ++ e ++ ")"
        showVarBinds bs e = "(" ++ intercalate ", " (showVarBind <$> bs) ++ " -> " ++ e ++ ")"
        showVarBind (n, e) = n ++ " = " ++ e
