module SICM.Symbolic.Integrate
    ( integrate
    , integrateTrace
    ) where

import Data.List (partition)
import Data.Ratio (denominator, numerator)

import SICM.Core.Expr (Expr(..))
import SICM.Symbolic.Differentiate (dependsOn, diff)
import SICM.Symbolic.Polynomial
    ( polynomialEquivalent
    , polynomialExpansion
    , polynomialNormalForm
    )
import SICM.Symbolic.Simplify (simplify)

-- | Find a symbolic antiderivative with respect to a symbolic variable.
--
-- The result deliberately uses 'Maybe':
--
--     Just antiderivative
--         means that one of the currently implemented strategies succeeded.
--
--     Nothing
--         means only that this integrator does not know how to solve the
--         integral yet.
--
-- The returned expression does not include an arbitrary additive constant.
--
-- The first version is intentionally modest but extensible.  It supports:
--
--   * constants and symbolic parameters
--   * sums
--   * constant multiples
--   * powers x^n, including rational n and n = -1
--   * polynomial normalization/expansion
--   * exp, sin, cos, sinh, cosh
--   * simple u-substitution
--   * conservative integration by parts for polynomial factors
--
-- More advanced rational integration, special functions, cyclic integration
-- by parts, and theorem-based definite integrals belong in later extensions.
integrate :: Expr -> Expr -> Maybe Expr
integrate variable expr
    | not (isVariable variable) =
        error "integrate: the integration variable must be a Symbol"

    | otherwise =
        simplify <$> integrateCore maximumDepth True variable (simplify expr)


-- | Integrate and return a short mathematical explanation.
--
-- The explanation intentionally records only mathematically meaningful
-- choices: expansion, substitution, integration by parts, and the final
-- standard integration rule.  Low-level simplifier rewrites stay silent.
integrateTrace :: Expr -> Expr -> Maybe (Expr, [String])
integrateTrace variable expr
    | not (isVariable variable) =
        error "integrateTrace: the integration variable must be a Symbol"

    | otherwise = do
        let input = simplify expr

        result <- integrate variable input

        pure
            ( result
            , integrationExplanation variable input result
            )

integrationExplanation :: Expr -> Expr -> Expr -> [String]
integrationExplanation variable expr result =
    case polynomialExpansion expr of
        Just expanded ->
            [ integralLabel variable expr
                ++ " = "
                ++ integralLabel variable expanded
                ++ "    [expand]"
            , integralLabel variable expanded
                ++ " = "
                ++ show result
                ++ "    [integrate terms]"
            ]

        Nothing ->
            case substitutionExplanation variable expr result of
                Just lines' ->
                    lines'

                Nothing ->
                    case partsExplanation variable expr result of
                        Just lines' ->
                            lines'

                        Nothing ->
                            [ integralLabel variable expr
                                ++ " = "
                                ++ show result
                                ++ "    ["
                                ++ basicIntegralReason variable expr
                                ++ "]"
                            ]

integralLabel :: Expr -> Expr -> String
integralLabel variable expr =
    "Integral_(" ++ show variable ++ ")(" ++ show expr ++ ")"

basicIntegralReason :: Expr -> Expr -> String
basicIntegralReason variable expr
    | not (dependsOn variable expr) =
        "constant rule"

    | expr == variable =
        "power rule"

    | otherwise =
        case expr of
            Add _ ->
                "linearity"

            Mul _ ->
                "constant multiple / standard rule"

            Pow base (Number _)
                | base == variable ->
                    "power rule"

            Apply name [argument]
                | argument == variable
                , name `elem` ["exp", "sin", "cos", "sinh", "cosh"] ->
                    "standard integral"

            _ ->
                "integration rule"

-- A depth limit protects a heuristic integrator from accidental search cycles.
maximumDepth :: Int
maximumDepth = 12

-- --------------------------------------------------------------------------
-- Strategy dispatcher
-- --------------------------------------------------------------------------

integrateCore :: Int -> Bool -> Expr -> Expr -> Maybe Expr
integrateCore depth allowParts variable expr
    | depth <= 0 =
        Nothing

    | otherwise =
        firstJust
            [ constantIntegral variable expr
            , sumIntegral depth allowParts variable expr
            , constantMultipleIntegral depth allowParts variable expr
            , powerIntegral variable expr
            , elementaryIntegral variable expr
            , polynomialIntegral depth allowParts variable expr
            , substitutionIntegral depth allowParts variable expr
            , if allowParts
                then partsIntegral depth variable expr
                else Nothing
            ]

firstJust :: [Maybe a] -> Maybe a
firstJust [] =
    Nothing
firstJust (result : rest) =
    case result of
        Just value -> Just value
        Nothing    -> firstJust rest

-- --------------------------------------------------------------------------
-- Level 1: direct rules
-- --------------------------------------------------------------------------

-- ∫ c dx = c*x, whenever c is independent of x.
constantIntegral :: Expr -> Expr -> Maybe Expr
constantIntegral variable expr
    | not (dependsOn variable expr) =
        Just (simplify (expr * variable))

    | otherwise =
        Nothing

-- ∫ (f + g + ...) dx = ∫f dx + ∫g dx + ...
sumIntegral :: Int -> Bool -> Expr -> Expr -> Maybe Expr
sumIntegral depth allowParts variable expr =
    case expr of
        Add terms -> do
            antiderivatives <-
                mapM
                    (integrateCore (depth - 1) allowParts variable)
                    terms

            pure (simplify (Add antiderivatives))

        _ ->
            Nothing

-- Pull every factor independent of x outside the integral:
--
--     ∫ c*f(x) dx = c ∫ f(x) dx
--
-- Symbolic parameters are allowed here because no division by them is
-- required.
constantMultipleIntegral :: Int -> Bool -> Expr -> Expr -> Maybe Expr
constantMultipleIntegral depth allowParts variable expr =
    case expr of
        Mul factors ->
            let (constants, varying) =
                    partition (not . dependsOn variable) factors

            in if null constants || null varying
                then
                    Nothing

                else do
                    inner <-
                        integrateCore
                            (depth - 1)
                            allowParts
                            variable
                            (makeProduct varying)

                    pure
                        (simplify
                            (makeProduct
                                [ makeProduct constants
                                , inner
                                ]))

        _ ->
            Nothing

-- Power rule:
--
--     ∫ x^n dx = x^(n+1)/(n+1),  n /= -1
--     ∫ x^-1 dx = log x
--
-- We use exact Rational exponents because Expr stores exact rationals.
powerIntegral :: Expr -> Expr -> Maybe Expr
powerIntegral variable expr
    | expr == variable =
        Just
            (Mul
                [ Number (1 / 2)
                , Pow variable (Number 2)
                ])

    | otherwise =
        case expr of
            Pow base (Number power)
                | base == variable ->
                    integratePower variable power

            _ ->
                Nothing

integratePower :: Expr -> Rational -> Maybe Expr
integratePower variable power
    | power == -1 =
        -- Formal real-domain convention for this first version.
        -- Assumption.hs can later distinguish log x from log |x|.
        Just (log variable)

    | otherwise =
        let newPower = power + 1
        in
            if newPower == 0
                then Nothing
                else
                    Just
                        (Mul
                            [ Number (1 / newPower)
                            , Pow variable (Number newPower)
                            ])

-- Elementary primitives for exactly the integration variable.
--
-- More general arguments such as sin(3*x) are handled by substitution.
elementaryIntegral :: Expr -> Expr -> Maybe Expr
elementaryIntegral variable expr =
    case expr of
        Apply "exp" [u]
            | u == variable ->
                Just (exp variable)

        Apply "sin" [u]
            | u == variable ->
                Just (negate (cos variable))

        Apply "cos" [u]
            | u == variable ->
                Just (sin variable)

        Apply "sinh" [u]
            | u == variable ->
                Just (cosh variable)

        Apply "cosh" [u]
            | u == variable ->
                Just (sinh variable)

        _ ->
            Nothing

-- --------------------------------------------------------------------------
-- Polynomial normalization
-- --------------------------------------------------------------------------

-- Polynomial normalization gives us distributive expansion and canonical
-- collection without turning Simplify.hs into a global algebra engine.
--
-- Example:
--
--     ∫ (x + 1)*(x - 1) dx
--
-- is first normalized to x^2 - 1 and then integrated term by term.
polynomialIntegral :: Int -> Bool -> Expr -> Expr -> Maybe Expr
polynomialIntegral depth allowParts variable expr =
    case polynomialNormalForm expr of
        Just normal
            | normal /= expr ->
                integrateCore
                    (depth - 1)
                    allowParts
                    variable
                    normal

        _ ->
            Nothing

-- --------------------------------------------------------------------------
-- Level 2: u-substitution
-- --------------------------------------------------------------------------

-- This implements the common pattern
--
--     ∫ c * u'(x) * F(u(x)) dx
--
-- where c is a safely recognized constant ratio.
--
-- Examples:
--
--     ∫ 2*x*exp(x^2) dx
--     ∫ x*cos(x^2) dx
--     ∫ x/(1+x^2) dx
--     ∫ sin(3*x) dx
--
-- The search is deliberately conservative.  We only divide by a derivative
-- coefficient when that coefficient is a known nonzero Number (or when the
-- two constant factors are exactly equal).  Symbolic nonzero assumptions are
-- deferred to Assumption.hs.
substitutionIntegral :: Int -> Bool -> Expr -> Expr -> Maybe Expr
substitutionIntegral _ _ variable expr =
    firstJust
        [ trySubstitution variable target remaining
        | (target, remaining) <- targetSplits expr
        ]

targetSplits :: Expr -> [(Expr, Expr)]
targetSplits expr =
    (expr, 1) :
        case expr of
            Mul factors ->
                splitFactors factors

            _ ->
                []

splitFactors :: [Expr] -> [(Expr, Expr)]
splitFactors factors =
    go [] factors
  where
    go _ [] =
        []

    go before (factor : after) =
        ( factor
        , makeProduct (reverse before ++ after)
        )
        : go (factor : before) after

trySubstitution :: Expr -> Expr -> Expr -> Maybe Expr
trySubstitution variable target remaining = do
    (_, _, _, _, result) <-
        substitutionDetails variable target remaining

    pure result

-- (inside, inside derivative, scale factor, primitive, result)
substitutionDetails
    :: Expr
    -> Expr
    -> Expr
    -> Maybe (Expr, Expr, Expr, Expr, Expr)
substitutionDetails variable target remaining = do
    (inside, primitive) <-
        outerPrimitive target

    if not (dependsOn variable inside)
        then
            Nothing

        else do
            let insideDerivative =
                    simplify (diff variable inside)

            ratio <-
                proportionalConstant
                    variable
                    remaining
                    insideDerivative

            let result =
                    simplify
                        (makeProduct
                            [ ratio
                            , primitive
                            ])

            pure
                ( inside
                , insideDerivative
                , ratio
                , primitive
                , result
                )

substitutionExplanation :: Expr -> Expr -> Expr -> Maybe [String]
substitutionExplanation variable expr expected =
    firstJust
        [ explain details
        | (target, remaining) <- targetSplits expr
        , Just details <-
            [substitutionDetails variable target remaining]
        ]
  where
    explain (inside, insideDerivative, _, _, candidate)
        | candidate /= expected =
            Nothing

        | otherwise =
            Just
                [ "u = "
                    ++ show inside
                    ++ ", du = "
                    ++ show insideDerivative
                    ++ " d"
                    ++ show variable
                    ++ "    [substitution]"
                , integralLabel variable expr
                    ++ " = "
                    ++ show expected
                    ++ "    [integrate and substitute back]"
                ]

-- Recognize F(u) and return (u, ∫F(u) du with u already substituted back).
outerPrimitive :: Expr -> Maybe (Expr, Expr)
outerPrimitive target =
    case target of
        Apply "exp" [u] ->
            Just (u, exp u)

        Apply "sin" [u] ->
            Just (u, negate (cos u))

        Apply "cos" [u] ->
            Just (u, sin u)

        Apply "sinh" [u] ->
            Just (u, cosh u)

        Apply "cosh" [u] ->
            Just (u, sinh u)

        Apply "sqrt" [u] ->
            Just
                ( u
                , Mul
                    [ Number (2 / 3)
                    , Pow u (Number (3 / 2))
                    ]
                )

        Pow u (Number power)
            | power == -1 ->
                Just (u, log u)

            | otherwise ->
                let newPower = power + 1
                in
                    if newPower == 0
                        then Nothing
                        else
                            Just
                                ( u
                                , Mul
                                    [ Number (1 / newPower)
                                    , Pow u (Number newPower)
                                    ]
                                )

        _ ->
            Nothing

-- Decide whether
--
--     numerator = constant * denominator
--
-- with a constant independent of the integration variable.
--
-- Both expressions are factored into an x-independent part and an
-- x-dependent part.  The dependent parts must be algebraically equal.
proportionalConstant :: Expr -> Expr -> Expr -> Maybe Expr
proportionalConstant variable ratioTop ratioBottom =
    let topNormal    = simplify ratioTop
        bottomNormal = simplify ratioBottom

        (numConstant, numVariablePart) =
            factorByVariable variable topNormal

        (denConstant, denVariablePart) =
            factorByVariable variable bottomNormal

    in
        if sameExpression numVariablePart denVariablePart
            then safeConstantQuotient numConstant denConstant
            else Nothing

factorByVariable :: Expr -> Expr -> (Expr, Expr)
factorByVariable variable expr =
    case expr of
        Mul factors ->
            let (constants, varying) =
                    partition (not . dependsOn variable) factors

            in
                ( simplify (makeProduct constants)
                , simplify (makeProduct varying)
                )

        _
            | dependsOn variable expr ->
                (1, expr)

            | otherwise ->
                (expr, 1)

sameExpression :: Expr -> Expr -> Bool
sameExpression lhs rhs =
    lhs == rhs
        || polynomialEquivalent lhs rhs == Just True

-- We avoid silently assuming a symbolic denominator is nonzero.
safeConstantQuotient :: Expr -> Expr -> Maybe Expr
safeConstantQuotient ratioTop ratioBottom
    | ratioTop == ratioBottom =
        Just 1

    | ratioBottom == 1 =
        Just ratioTop

    | otherwise =
        case ratioBottom of
            Number value
                | value /= 0 ->
                    Just (simplify (ratioTop / ratioBottom))

            _ ->
                Nothing

-- --------------------------------------------------------------------------
-- Level 2: conservative integration by parts
-- --------------------------------------------------------------------------

-- For now we use integration by parts only when one factor is polynomial in
-- x.  This captures an important and well-behaved family:
--
--     x * exp x
--     x^2 * exp x
--     x * sin x
--     (x^2 + 1) * cos x
--
-- If
--
--     ∫ u dv = u*v - ∫ v du
--
-- the chosen u must have positive polynomial degree.  Differentiating it
-- therefore lowers that degree, giving the search a natural direction.
partsIntegral :: Int -> Expr -> Expr -> Maybe Expr
partsIntegral depth variable expr =
    case expr of
        Mul factors ->
            firstJust
                [ tryParts depth variable u dv
                | (u, dv) <- splitFactors factors
                , hasPositivePolynomialDegree variable u
                ]

        _ ->
            Nothing

tryParts :: Int -> Expr -> Expr -> Expr -> Maybe Expr
tryParts depth variable u dv = do
    (_, _, _, _, _, _, result) <-
        partsDetails depth variable u dv

    pure result

-- (u, dv, v, du, remainder integrand, remainder antiderivative, result)
partsDetails
    :: Int
    -> Expr
    -> Expr
    -> Expr
    -> Maybe (Expr, Expr, Expr, Expr, Expr, Expr, Expr)
partsDetails depth variable u dv = do
    -- Do not let the first integral recursively choose integration by parts;
    -- dv should be something our simpler strategies already know.
    v <-
        integrateCore
            (depth - 1)
            False
            variable
            dv

    let du =
            simplify (diff variable u)

    if du == 0
        then
            Nothing

        else do
            let remainderIntegrand =
                    simplify (v * du)

            remainder <-
                integrateCore
                    (depth - 1)
                    True
                    variable
                    remainderIntegrand

            let result =
                    simplify (u * v - remainder)

            pure
                ( u
                , dv
                , v
                , du
                , remainderIntegrand
                , remainder
                , result
                )

partsExplanation :: Expr -> Expr -> Expr -> Maybe [String]
partsExplanation variable expr expected =
    case expr of
        Mul factors ->
            firstJust
                [ explain details
                | (u, dv) <- splitFactors factors
                , hasPositivePolynomialDegree variable u
                , Just details <-
                    [partsDetails maximumDepth variable u dv]
                ]

        _ ->
            Nothing
  where
    explain (u, dv, v, du, remainderIntegrand, _, candidate)
        | candidate /= expected =
            Nothing

        | otherwise =
            Just
                [ "u = "
                    ++ show u
                    ++ ", dv = "
                    ++ show dv
                    ++ " d"
                    ++ show variable
                    ++ ", v = "
                    ++ show v
                    ++ ", du = "
                    ++ show du
                    ++ " d"
                    ++ show variable
                , integralLabel variable expr
                    ++ " = "
                    ++ show (simplify (u * v))
                    ++ " - "
                    ++ integralLabel variable remainderIntegrand
                    ++ "    [integration by parts]"
                , "= "
                    ++ show expected
                    ++ "    [integrate remainder; simplify]"
                ]

-- Degree in the chosen integration variable only.  Other symbols are treated
-- as coefficients/parameters.
polynomialDegreeIn :: Expr -> Expr -> Maybe Integer
polynomialDegreeIn variable expr
    | not (dependsOn variable expr) =
        Just 0

    | expr == variable =
        Just 1

    | otherwise =
        case expr of
            Add terms ->
                maximumMaybe
                    =<< mapM (polynomialDegreeIn variable) terms

            Mul factors ->
                sum
                    <$> mapM (polynomialDegreeIn variable) factors

            Pow base (Number power)
                | denominator power == 1
                , let n = numerator power
                , n >= 0 -> do
                    baseDegree <-
                        polynomialDegreeIn variable base

                    pure (n * baseDegree)

            _ ->
                Nothing

hasPositivePolynomialDegree :: Expr -> Expr -> Bool
hasPositivePolynomialDegree variable expr =
    case polynomialDegreeIn variable expr of
        Just degree -> degree > 0
        Nothing     -> False

maximumMaybe :: [Integer] -> Maybe Integer
maximumMaybe [] =
    Just 0
maximumMaybe values =
    Just (maximum values)

-- --------------------------------------------------------------------------
-- Small structural helpers
-- --------------------------------------------------------------------------

makeProduct :: [Expr] -> Expr
makeProduct [] =
    1
makeProduct [factor] =
    factor
makeProduct factors =
    Mul factors

isVariable :: Expr -> Bool
isVariable (Symbol _) = True
isVariable _          = False
