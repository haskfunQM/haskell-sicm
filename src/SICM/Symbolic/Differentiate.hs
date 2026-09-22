module SICM.Symbolic.Differentiate
    ( differentiate
    , diff
    , diffTrace
    , dependsOn
    ) where

import SICM.Core.Expr (Expr(..))
import SICM.Symbolic.Simplify (simplify)

differentiate :: Expr -> Expr -> Expr
differentiate variable expr
    | not (isVariable variable) =
        error "differentiate: the differentiation variable must be a Symbol"
    | otherwise =
        derivative expr
  where
    derivative current =
        case current of
            Number _ ->
                0

            Symbol _
                | current == variable -> 1
                | otherwise           -> 0

            Add terms ->
                Add (map derivative terms)

            Mul factors ->
                differentiateProduct derivative factors

            Pow base power ->
                differentiatePower derivative variable base power

            Apply name arguments ->
                differentiateFunction derivative variable name arguments

diff :: Expr -> Expr -> Expr
diff variable =
    simplify . differentiate variable


-- | Differentiate and return a short mathematical explanation.
--
-- The trace deliberately ignores low-level simplifier rewrites.  It records
-- only the calculus rule used at the outermost expression, followed by one
-- silent-normalization step when simplification changes the raw derivative.
--
-- Example:
--
--     diffTrace x (x*x*x)
--
-- explains the product rule and then the simplification to 3*x^2.
diffTrace :: Expr -> Expr -> (Expr, [String])
diffTrace variable expr
    | not (isVariable variable) =
        error "diffTrace: the differentiation variable must be a Symbol"

    | otherwise =
        let rawDerivative =
                differentiate variable expr

            result =
                simplify rawDerivative

            calculusLine =
                derivativeLabel variable expr
                    ++ " = "
                    ++ show rawDerivative
                    ++ "    ["
                    ++ derivativeReason variable expr
                    ++ "]"

            trace
                | rawDerivative == result =
                    [calculusLine]

                | otherwise =
                    [ calculusLine
                    , show rawDerivative
                        ++ " = "
                        ++ show result
                        ++ "    [simplify]"
                    ]

        in (result, trace)

derivativeLabel :: Expr -> Expr -> String
derivativeLabel variable expr =
    "D_" ++ show variable ++ "(" ++ show expr ++ ")"

derivativeReason :: Expr -> Expr -> String
derivativeReason variable expr =
    case expr of
        Number _ ->
            "constant rule"

        Symbol _
            | expr == variable ->
                "D(" ++ show variable ++ ") = 1"

            | otherwise ->
                "independent symbol"

        Add _ ->
            "sum rule"

        Mul _ ->
            "product rule"

        Pow base power
            | base == power ->
                "logarithmic differentiation"

            | not (dependsOn variable power) ->
                "power rule"

            | not (dependsOn variable base) ->
                "exponential rule"

            | otherwise ->
                "logarithmic differentiation"

        Apply name arguments
            | name `elem` elementaryFunctions
            , any (dependsOn variable) arguments ->
                "chain rule"

            | any (dependsOn variable) arguments ->
                "formal derivative"

            | otherwise ->
                "constant rule"

elementaryFunctions :: [String]
elementaryFunctions =
    [ "sin", "cos", "tan"
    , "exp", "log", "sqrt"
    , "asin", "acos", "atan"
    , "sinh", "cosh", "tanh"
    , "asinh", "acosh", "atanh"
    ]

dependsOn :: Expr -> Expr -> Bool
dependsOn variable expr =
    case expr of
        Number _ ->
            False

        Symbol _ ->
            expr == variable

        Add terms ->
            any (dependsOn variable) terms

        Mul factors ->
            any (dependsOn variable) factors

        Pow base power ->
            dependsOn variable base || dependsOn variable power

        Apply _ arguments ->
            any (dependsOn variable) arguments

differentiateProduct :: (Expr -> Expr) -> [Expr] -> Expr
differentiateProduct _ [] =
    0
differentiateProduct derivative factors =
    Add
        [ Mul (before ++ [derivative factor] ++ after)
        | (before, factor, after) <- splitEach factors
        ]

splitEach :: [a] -> [([a], a, [a])]
splitEach =
    go []
  where
    go _ [] =
        []

    go before (x : xs) =
        (reverse before, x, xs) : go (x : before) xs

differentiatePower
    :: (Expr -> Expr)
    -> Expr
    -> Expr
    -> Expr
    -> Expr
differentiatePower derivative variable base power
    -- u^u has a useful direct form that avoids manufacturing u/u:
    --
    --     D(u^u) = u^u * Du * (log u + 1)
    | base == power =
        (base ** power)
            * derivative base
            * (log base + 1)

    -- The exponent is constant with respect to the chosen variable:
    --
    --     D(u^n) = n * u^(n-1) * Du
    | not (dependsOn variable power) =
        power * (base ** (power - 1)) * derivative base

    -- The base is constant:
    --
    --     D(a^v) = a^v * log(a) * Dv
    | not (dependsOn variable base) =
        derivative power * log base * (base ** power)

    -- General logarithmic differentiation:
    --
    --     D(u^v) = u^v * (Dv*log u + v*Du/u)
    | otherwise =
        (base ** power)
            * ( derivative power * log base
              + power * derivative base / base
              )

differentiateFunction
    :: (Expr -> Expr)
    -> Expr
    -> String
    -> [Expr]
    -> Expr
differentiateFunction derivative variable name arguments =
    case (name, arguments) of
        ("sin", [u]) ->
            derivative u * cos u

        ("cos", [u]) ->
            negate (derivative u * sin u)

        ("tan", [u]) ->
            derivative u / (cos u ** 2)

        ("exp", [u]) ->
            derivative u * exp u

        ("log", [u]) ->
            derivative u / u

        ("sqrt", [u]) ->
            derivative u / (2 * sqrt u)

        ("asin", [u]) ->
            derivative u / sqrt (1 - u ** 2)

        ("acos", [u]) ->
            negate (derivative u / sqrt (1 - u ** 2))

        ("atan", [u]) ->
            derivative u / (1 + u ** 2)

        ("sinh", [u]) ->
            derivative u * cosh u

        ("cosh", [u]) ->
            derivative u * sinh u

        ("tanh", [u]) ->
            derivative u / (cosh u ** 2)

        ("asinh", [u]) ->
            derivative u / sqrt (1 + u ** 2)

        ("acosh", [u]) ->
            derivative u / (sqrt (u - 1) * sqrt (u + 1))

        ("atanh", [u]) ->
            derivative u / (1 - u ** 2)

        _
            | any (dependsOn variable) arguments ->
                Apply "diff" [variable, Apply name arguments]

            | otherwise ->
                0

isVariable :: Expr -> Bool
isVariable (Symbol _) = True
isVariable _          = False
