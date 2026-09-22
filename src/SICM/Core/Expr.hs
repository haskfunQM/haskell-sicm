module SICM.Core.Expr
    ( Name
    , Expr(..)
    , symbol
    , symbols
    ) where

import Data.Ratio (denominator, numerator)

-- | Names of symbolic variables and symbolic functions.
type Name = String

-- | The core symbolic expression tree.
--
-- The constructors are deliberately small and mathematical.  Most users
-- should build expressions with ordinary Haskell syntax such as
--
--     x + y
--     x * y
--     (x + 1) / 2
--     sin x
--
-- rather than constructing 'Add', 'Mul', and so on directly.
data Expr
    = Number Rational
    | Symbol Name
    | Add [Expr]
    | Mul [Expr]
    | Pow Expr Expr
    | Apply Name [Expr]
    deriving (Eq, Ord)

-- | Construct a symbolic variable.
symbol :: Name -> Expr
symbol = Symbol

-- | Construct several symbolic variables at once.
symbols :: [Name] -> [Expr]
symbols = map symbol

instance Num Expr where
    x + y = Add [x, y]
    x * y = Mul [x, y]
    negate x = Mul [Number (-1), x]

    fromInteger = Number . fromInteger

    abs x = Apply "abs" [x]
    signum x = Apply "signum" [x]

instance Fractional Expr where
    fromRational = Number
    recip x = Pow x (Number (-1))
    x / y = Mul [x, recip y]

instance Floating Expr where
    pi = Symbol "pi"

    exp x  = Apply "exp"  [x]
    log x  = Apply "log"  [x]
    sqrt x = Apply "sqrt" [x]

    sin x = Apply "sin" [x]
    cos x = Apply "cos" [x]
    tan x = Apply "tan" [x]

    asin x = Apply "asin" [x]
    acos x = Apply "acos" [x]
    atan x = Apply "atan" [x]

    sinh x = Apply "sinh" [x]
    cosh x = Apply "cosh" [x]
    tanh x = Apply "tanh" [x]

    asinh x = Apply "asinh" [x]
    acosh x = Apply "acosh" [x]
    atanh x = Apply "atanh" [x]

    x ** y = Pow x y
    logBase x y = log y / log x

instance Show Expr where
    show = showExpr

showExpr :: Expr -> String
showExpr expr =
    case expr of
        Number n -> showRational n
        Symbol name -> name
        Add xs -> parenthesize (joinWith " + " (map showExpr xs))
        Mul xs -> parenthesize (joinWith " * " (map showExpr xs))
        Pow base exponent ->
            parenthesize (showExpr base ++ " ^ " ++ showExpr exponent)
        Apply name args ->
            name ++ "(" ++ joinWith ", " (map showExpr args) ++ ")"

showRational :: Rational -> String
showRational r
    | denominator r == 1 = show (numerator r)
    | otherwise = show (numerator r) ++ "/" ++ show (denominator r)

parenthesize :: String -> String
parenthesize text = "(" ++ text ++ ")"

joinWith :: String -> [String] -> String
joinWith _ [] = ""
joinWith _ [x] = x
joinWith separator (x : xs) = x ++ separator ++ joinWith separator xs
