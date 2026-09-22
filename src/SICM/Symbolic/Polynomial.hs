module SICM.Symbolic.Polynomial
    ( polynomialNormalForm
    , polynomialEquivalent
    , polynomialExpansion
    , isPolynomial
    , algebraicNormalForm
    , algebraicEquivalent
    ) where

import Data.List (groupBy, nub, sort, sortBy)
import Data.Maybe (mapMaybe)
import Data.Ratio (denominator, numerator)

import SICM.Core.Expr (Expr(..))

newtype Monomial =
    Monomial [(Expr, Integer)]
    deriving (Eq, Show)

newtype Polynomial =
    Polynomial [(Monomial, Rational)]
    deriving (Eq, Show)

-- | Convert a strict polynomial expression into a canonical Expr.
--
-- Function applications such as sin(x) are deliberately rejected here.
polynomialNormalForm :: Expr -> Maybe Expr
polynomialNormalForm expr =
    polynomialToExpr <$> expressionToPolynomial expr

-- | Decide strict polynomial equality.
polynomialEquivalent :: Expr -> Expr -> Maybe Bool
polynomialEquivalent lhs rhs = do
    leftPolynomial <- expressionToPolynomial lhs
    rightPolynomial <- expressionToPolynomial rhs
    pure (leftPolynomial == rightPolynomial)

isPolynomial :: Expr -> Bool
isPolynomial expr =
    case expressionToPolynomial expr of
        Just _ ->
            True

        Nothing ->
            False

-- | Canonical algebraic form used internally by the prover.
--
-- Unlike 'polynomialNormalForm', function applications are treated as
-- indivisible algebraic atoms.  Thus sin(theta) is not claimed to be a
-- polynomial in theta, but an expression such as
--
--     (a * sin(theta) + b * cos(theta))^2
--
-- can still be expanded algebraically.
algebraicNormalForm :: Expr -> Maybe Expr
algebraicNormalForm expr =
    polynomialToExpr <$> expressionToPolynomialWithAtoms expr

-- | Algebraic equivalence with function applications treated as opaque atoms.
algebraicEquivalent :: Expr -> Expr -> Maybe Bool
algebraicEquivalent lhs rhs = do
    leftPolynomial <- expressionToPolynomialWithAtoms lhs
    rightPolynomial <- expressionToPolynomialWithAtoms rhs
    pure (leftPolynomial == rightPolynomial)

-- | Expand an expression only when genuine distributive/power expansion is
-- involved.
polynomialExpansion :: Expr -> Maybe Expr
polynomialExpansion expr
    | not (needsExpansion expr) =
        Nothing

    | otherwise = do
        normal <- polynomialNormalForm expr

        if normal == expr
            then Nothing
            else Just normal

needsExpansion :: Expr -> Bool
needsExpansion expr =
    case expr of
        Number _ ->
            False

        Symbol _ ->
            False

        Add terms ->
            any needsExpansion terms

        Mul factors ->
            any isSum factors
                || any needsExpansion factors

        Pow base power ->
            powerExpands base power
                || needsExpansion base
                || needsExpansion power

        Apply _ arguments ->
            any needsExpansion arguments
  where
    isSum (Add _) =
        True

    isSum _ =
        False

    powerExpands base (Number power)
        | denominator power == 1
        , numerator power > 1 =
            containsSum base

    powerExpands _ _ =
        False

    containsSum (Add _) =
        True

    containsSum (Mul factors) =
        any containsSum factors

    containsSum (Pow base power) =
        containsSum base || containsSum power

    containsSum (Apply _ arguments) =
        any containsSum arguments

    containsSum _ =
        False

-- --------------------------------------------------------------------------
-- Expr -> Polynomial
-- --------------------------------------------------------------------------

expressionToPolynomial :: Expr -> Maybe Polynomial
expressionToPolynomial =
    expressionToPolynomialUsing False

expressionToPolynomialWithAtoms :: Expr -> Maybe Polynomial
expressionToPolynomialWithAtoms =
    expressionToPolynomialUsing True

expressionToPolynomialUsing :: Bool -> Expr -> Maybe Polynomial
expressionToPolynomialUsing allowFunctionAtoms expr =
    case expr of
        Number coefficient ->
            Just (constantPolynomial coefficient)

        Symbol _ ->
            Just (atomPolynomial expr)

        Add terms ->
            foldl polynomialAdd zeroPolynomial
                <$> mapM (expressionToPolynomialUsing allowFunctionAtoms) terms

        Mul factors ->
            foldl polynomialMultiply onePolynomial
                <$> mapM (expressionToPolynomialUsing allowFunctionAtoms) factors

        Pow base power ->
            case power of
                Number value
                    | denominator value == 1
                    , let n = numerator value
                    , n >= 0 -> do
                        basePolynomial <-
                            expressionToPolynomialUsing allowFunctionAtoms base

                        pure (polynomialPower basePolynomial n)

                _ ->
                    Nothing

        Apply _ _
            | allowFunctionAtoms ->
                Just (atomPolynomial expr)

            | otherwise ->
                Nothing

-- --------------------------------------------------------------------------
-- Canonical polynomial arithmetic
-- --------------------------------------------------------------------------

zeroPolynomial :: Polynomial
zeroPolynomial =
    Polynomial []

onePolynomial :: Polynomial
onePolynomial =
    constantPolynomial 1

constantPolynomial :: Rational -> Polynomial
constantPolynomial 0 =
    zeroPolynomial

constantPolynomial coefficient =
    Polynomial [(Monomial [], coefficient)]

atomPolynomial :: Expr -> Polynomial
atomPolynomial atom =
    Polynomial [(Monomial [(atom, 1)], 1)]

polynomialAdd :: Polynomial -> Polynomial -> Polynomial
polynomialAdd (Polynomial lhs) (Polynomial rhs) =
    normalizePolynomial (lhs ++ rhs)

polynomialMultiply :: Polynomial -> Polynomial -> Polynomial
polynomialMultiply (Polynomial lhs) (Polynomial rhs) =
    normalizePolynomial
        [ ( multiplyMonomials leftMonomial rightMonomial
          , leftCoefficient * rightCoefficient
          )
        | (leftMonomial, leftCoefficient) <- lhs
        , (rightMonomial, rightCoefficient) <- rhs
        ]

polynomialPower :: Polynomial -> Integer -> Polynomial
polynomialPower _ 0 =
    onePolynomial

polynomialPower polynomial 1 =
    polynomial

polynomialPower polynomial n
    | even n =
        let half =
                polynomialPower polynomial (n `div` 2)
        in
            polynomialMultiply half half

    | otherwise =
        polynomialMultiply
            polynomial
            (polynomialPower polynomial (n - 1))

multiplyMonomials :: Monomial -> Monomial -> Monomial
multiplyMonomials (Monomial lhs) (Monomial rhs) =
    normalizeMonomial (lhs ++ rhs)

normalizeMonomial :: [(Expr, Integer)] -> Monomial
normalizeMonomial powers =
    Monomial (mapMaybe combinePowerGroup grouped)
  where
    grouped =
        groupBy sameAtom
            (sortBy compareAtom powers)

    compareAtom (leftAtom, _) (rightAtom, _) =
        compare leftAtom rightAtom

    sameAtom (leftAtom, _) (rightAtom, _) =
        leftAtom == rightAtom

    combinePowerGroup [] =
        Nothing

    combinePowerGroup group@((atom, _) : _) =
        let total =
                sum (map snd group)
        in
            if total == 0
                then Nothing
                else Just (atom, total)

normalizePolynomial :: [(Monomial, Rational)] -> Polynomial
normalizePolynomial terms =
    Polynomial (mapMaybe combineTermGroup grouped)
  where
    grouped =
        groupBy sameMonomial
            (sortBy compareTerm terms)

    compareTerm (leftMonomial, _) (rightMonomial, _) =
        compareMonomial leftMonomial rightMonomial

    sameMonomial (leftMonomial, _) (rightMonomial, _) =
        leftMonomial == rightMonomial

    combineTermGroup [] =
        Nothing

    combineTermGroup group@((monomial, _) : _) =
        let coefficient =
                sum (map snd group)
        in
            if coefficient == 0
                then Nothing
                else Just (monomial, coefficient)

-- | Canonical term order:
--   1. higher total degree first;
--   2. for equal degree, earlier atoms get priority.
compareMonomial :: Monomial -> Monomial -> Ordering
compareMonomial left right =
    case compare (monomialDegree right) (monomialDegree left) of
        EQ ->
            compareExponentVectors left right

        ordering ->
            ordering

monomialDegree :: Monomial -> Integer
monomialDegree (Monomial powers) =
    sum (map snd powers)

compareExponentVectors :: Monomial -> Monomial -> Ordering
compareExponentVectors left@(Monomial lhs) right@(Monomial rhs) =
    compareVectors
        [ exponentOf atom left  | atom <- atoms ]
        [ exponentOf atom right | atom <- atoms ]
  where
    atoms =
        sort . nub $
            map fst lhs ++ map fst rhs

    compareVectors [] [] =
        EQ

    compareVectors (x : xs) (y : ys) =
        case compare y x of
            EQ ->
                compareVectors xs ys

            ordering ->
                ordering

    compareVectors _ _ =
        EQ

exponentOf :: Expr -> Monomial -> Integer
exponentOf atom (Monomial powers) =
    case lookup atom powers of
        Just power ->
            power

        Nothing ->
            0

-- --------------------------------------------------------------------------
-- Polynomial -> Expr
-- --------------------------------------------------------------------------

polynomialToExpr :: Polynomial -> Expr
polynomialToExpr (Polynomial []) =
    0

polynomialToExpr (Polynomial terms) =
    makeSum (map termToExpr terms)

termToExpr :: (Monomial, Rational) -> Expr
termToExpr (Monomial [], coefficient) =
    Number coefficient

termToExpr (monomial, coefficient)
    | coefficient == 1 =
        monomialToExpr monomial

    | otherwise =
        makeProduct
            [ Number coefficient
            , monomialToExpr monomial
            ]

monomialToExpr :: Monomial -> Expr
monomialToExpr (Monomial powers) =
    makeProduct
        [ atomPower atom power
        | (atom, power) <- powers
        ]

atomPower :: Expr -> Integer -> Expr
atomPower atom 1 =
    atom

atomPower atom power =
    Pow
        atom
        (Number (fromIntegral power))

makeSum :: [Expr] -> Expr
makeSum [] =
    0

makeSum [term] =
    term

makeSum terms =
    Add terms

makeProduct :: [Expr] -> Expr
makeProduct [] =
    1

makeProduct [factor] =
    factor

makeProduct factors =
    Mul factors
