module SICM.Symbolic.Identity
    ( reduceIdentities
    , expandIdentities
    ) where

import SICM.Core.Expr (Expr(..))
import SICM.Symbolic.Rewrite
    ( Rule
    , rewriteRules
    , rule
    )

-- | Identities that move toward a smaller/canonical expression.
reduceIdentities :: Expr -> Expr
reduceIdentities =
    rewriteRules reductionRules

-- | Identities that deliberately expand a compound trigonometric argument.
--
-- These are used by the prover, not by the ordinary simplifier, because
-- expansion is not always a simplification.
expandIdentities :: Expr -> Expr
expandIdentities =
    rewriteRules expansionRules

reductionRules :: [Rule]
reductionRules =
    [ sinNegativeRule
    , cosNegativeRule
    , sinZeroRule
    , cosZeroRule
    , sinPiOverTwoRule
    , cosPiOverTwoRule
    , sinPiRule
    , cosPiRule
    , expZeroRule
    , logExpRule

      -- Prover-side canonical form of the Pythagorean identity:
      --
      --     cos(x)^2 = 1 - sin(x)^2
      --
      -- This single orientation lets polynomial normalization prove both
      -- sin^2+cos^2=1 and larger identities that contain the same relation.
    , cosSquaredRule
    ]

expansionRules :: [Rule]
expansionRules =
    [ sinDoubleAngleRule
    , cosDoubleAngleRule
    , sinAdditionRule
    , cosAdditionRule
    ]

sinNegativeRule :: Rule
sinNegativeRule =
    rule "sin-negative" $ \expr ->
        case expr of
            Apply "sin" [argument] ->
                case splitNegative argument of
                    Just positive ->
                        Just (-sin positive)

                    Nothing ->
                        Nothing

            _ ->
                Nothing

cosNegativeRule :: Rule
cosNegativeRule =
    rule "cos-negative" $ \expr ->
        case expr of
            Apply "cos" [argument] ->
                case splitNegative argument of
                    Just positive ->
                        Just (cos positive)

                    Nothing ->
                        Nothing

            _ ->
                Nothing

sinZeroRule :: Rule
sinZeroRule =
    rule "sin-zero" $ \expr ->
        case expr of
            Apply "sin" [Number 0] ->
                Just 0

            _ ->
                Nothing

cosZeroRule :: Rule
cosZeroRule =
    rule "cos-zero" $ \expr ->
        case expr of
            Apply "cos" [Number 0] ->
                Just 1

            _ ->
                Nothing

sinPiOverTwoRule :: Rule
sinPiOverTwoRule =
    rule "sin-pi-over-two" $ \expr ->
        case expr of
            Apply "sin" [argument]
                | isPiOverTwo argument ->
                    Just 1

            _ ->
                Nothing

cosPiOverTwoRule :: Rule
cosPiOverTwoRule =
    rule "cos-pi-over-two" $ \expr ->
        case expr of
            Apply "cos" [argument]
                | isPiOverTwo argument ->
                    Just 0

            _ ->
                Nothing

sinPiRule :: Rule
sinPiRule =
    rule "sin-pi" $ \expr ->
        case expr of
            Apply "sin" [Symbol "pi"] ->
                Just 0

            _ ->
                Nothing

cosPiRule :: Rule
cosPiRule =
    rule "cos-pi" $ \expr ->
        case expr of
            Apply "cos" [Symbol "pi"] ->
                Just (-1)

            _ ->
                Nothing

expZeroRule :: Rule
expZeroRule =
    rule "exp-zero" $ \expr ->
        case expr of
            Apply "exp" [Number 0] ->
                Just 1

            _ ->
                Nothing

logExpRule :: Rule
logExpRule =
    rule "log-exp" $ \expr ->
        case expr of
            Apply "log" [Apply "exp" [argument]] ->
                Just argument

            _ ->
                Nothing

-- | Canonical orientation of sin^2(x)+cos^2(x)=1.
--
-- The prover uses this rather than installing a global simplifier rule that
-- would have to recognize every possible factored context.
cosSquaredRule :: Rule
cosSquaredRule =
    rule "cos-squared" $ \expr ->
        case expr of
            Pow (Apply "cos" [argument]) (Number 2) ->
                Just (1 - sin argument ** 2)

            _ ->
                Nothing

sinAdditionRule :: Rule
sinAdditionRule =
    rule "sin-addition" $ \expr ->
        case expr of
            Apply "sin" [Add terms] ->
                case splitSum terms of
                    Just (left, right) ->
                        Just
                            ( sin left * cos right
                            + cos left * sin right
                            )

                    Nothing ->
                        Nothing

            _ ->
                Nothing

cosAdditionRule :: Rule
cosAdditionRule =
    rule "cos-addition" $ \expr ->
        case expr of
            Apply "cos" [Add terms] ->
                case splitSum terms of
                    Just (left, right) ->
                        Just
                            ( cos left * cos right
                            - sin left * sin right
                            )

                    Nothing ->
                        Nothing

            _ ->
                Nothing

sinDoubleAngleRule :: Rule
sinDoubleAngleRule =
    rule "sin-double-angle" $ \expr ->
        case expr of
            Apply "sin" [argument] ->
                case splitDouble argument of
                    Just x ->
                        Just (2 * sin x * cos x)

                    Nothing ->
                        Nothing

            _ ->
                Nothing

cosDoubleAngleRule :: Rule
cosDoubleAngleRule =
    rule "cos-double-angle" $ \expr ->
        case expr of
            Apply "cos" [argument] ->
                case splitDouble argument of
                    Just x ->
                        Just (cos x ** 2 - sin x ** 2)

                    Nothing ->
                        Nothing

            _ ->
                Nothing

splitNegative :: Expr -> Maybe Expr
splitNegative expr =
    case expr of
        Mul (Number coefficient : factors)
            | coefficient == -1 ->
                Just (makeProduct factors)

        Mul [factor, Number coefficient]
            | coefficient == -1 ->
                Just factor

        _ ->
            Nothing

splitDouble :: Expr -> Maybe Expr
splitDouble expr =
    case expr of
        Mul (Number coefficient : factors)
            | coefficient == 2
            , not (null factors) ->
                Just (makeProduct factors)

        Mul [factor, Number coefficient]
            | coefficient == 2 ->
                Just factor

        _ ->
            Nothing

splitSum :: [Expr] -> Maybe (Expr, Expr)
splitSum terms =
    case terms of
        first : second : rest ->
            Just (first, makeSum (second : rest))

        _ ->
            Nothing

isPiOverTwo :: Expr -> Bool
isPiOverTwo expr =
    case expr of
        Mul [Number coefficient, Symbol "pi"] ->
            coefficient == 1 / 2

        Mul [Symbol "pi", Number coefficient] ->
            coefficient == 1 / 2

        Mul
            [ Symbol "pi"
            , Pow (Number 2) (Number (-1))
            ] ->
                True

        Mul
            [ Pow (Number 2) (Number (-1))
            , Symbol "pi"
            ] ->
                True

        _ ->
            False

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
