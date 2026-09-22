module SICM.Symbolic.Simplify
    ( simplify
    , basicRules
    , flattenAddRule
    , flattenMulRule
    , numericPowerRule
    , zeroProductRule
    , collectAddNumbersRule
    , collectMulNumbersRule
    , collectLikeTermsRule
    , combineRepeatedFactorsRule
    , removeAddZeroRule
    , removeMulOneRule
    , powerOneRule
    , collapseAddRule
    , collapseMulRule
    ) where

import Data.List (partition)
import Data.Ratio (denominator, numerator)

import SICM.Core.Expr (Expr(..))
import SICM.Symbolic.Rewrite
    ( Rule
    , productRule
    , rewriteRules
    , rule
    , sumRule
    )

simplify :: Expr -> Expr
simplify = rewriteRules basicRules

basicRules :: [Rule]
basicRules =
    [ flattenAddRule
    , flattenMulRule
    , numericPowerRule
    , zeroProductRule
    , collectAddNumbersRule
    , collectMulNumbersRule
    , removeAddZeroRule
    , removeMulOneRule
    , collectLikeTermsRule
    , combineRepeatedFactorsRule
    , powerOneRule
    , collapseAddRule
    , collapseMulRule
    ]

flattenAddRule :: Rule
flattenAddRule =
    sumRule "flatten-add" $
        Just . Add . concatMap flatten
  where
    flatten (Add terms) = terms
    flatten term        = [term]

flattenMulRule :: Rule
flattenMulRule =
    productRule "flatten-mul" $
        Just . Mul . concatMap flatten
  where
    flatten (Mul factors) = factors
    flatten factor        = [factor]

numericPowerRule :: Rule
numericPowerRule =
    rule "numeric-power" $ \expr ->
        case expr of
            Pow (Number base) (Number power)
                | denominator power == 1
                , let n = numerator power
                , not (base == 0 && n <= 0) ->
                    Just (Number (base ^^ n))
            _ -> Nothing

zeroProductRule :: Rule
zeroProductRule =
    productRule "zero-product" $ \factors ->
        if 0 `elem` factors
            then Just 0
            else Nothing

collectAddNumbersRule :: Rule
collectAddNumbersRule =
    sumRule "collect-add-numbers" $ \terms ->
        let numbers = [n | Number n <- terms]
            others  = [term | term <- terms, not (isNumber term)]
        in case numbers of
            _ : _ : _ ->
                let total = sum numbers
                    result
                        | total == 0 = others
                        | otherwise  = others ++ [Number total]
                in Just (Add result)
            _ -> Nothing

collectMulNumbersRule :: Rule
collectMulNumbersRule =
    productRule "collect-mul-numbers" $ \factors ->
        let numericFactors =
                [ value | Number value <- factors ]

            otherFactors =
                [ factor
                | factor <- factors
                , not (isNumber factor)
                ]

        in case numericFactors of
            [] ->
                Nothing

            _ ->
                let coefficient =
                        product numericFactors

                    result
                        | coefficient == 1 =
                            otherFactors

                        | otherwise =
                            Number coefficient : otherFactors

                in Just (Mul result)

-- | Collect terms only when two or more terms really have the same
-- symbolic body.
--
--     x + x           -> 2*x
--     2*x + x         -> 3*x
--     2*x^2 + x^2     -> 3*x^2
--
-- An unmatched term is preserved exactly as it was.  This is important:
-- merely decomposing and rebuilding a lone product can change its tree
-- shape and create a rewrite cycle with 'flattenMulRule'.
collectLikeTermsRule :: Rule
collectLikeTermsRule =
    sumRule "collect-like-terms" $
        Just . Add . collect
  where
    collect [] =
        []

    collect (term : rest)
        | isNumber term =
            term : collect rest

        | otherwise =
            let (coefficient, body) = splitCoefficient term

                sameBody candidate =
                    not (isNumber candidate)
                        && snd (splitCoefficient candidate) == body

                (same, different) =
                    partition sameBody rest

            in if null same
                then
                    -- No algebra happened: preserve the original tree.
                    term : collect rest

                else
                    let total =
                            coefficient
                                + sum
                                    [ c
                                    | candidate <- same
                                    , let (c, _) = splitCoefficient candidate
                                    ]

                        combined =
                            makeTerm total body

                    in case combined of
                        Number 0 -> collect different
                        _        -> combined : collect different

splitCoefficient :: Expr -> (Rational, Expr)
splitCoefficient expr =
    case expr of
        Mul factors ->
            let numbers = [n | Number n <- factors]
                others  = [factor | factor <- factors, not (isNumber factor)]
                coefficient =
                    if null numbers
                        then 1
                        else product numbers
            in (coefficient, makeProduct others)

        _ ->
            (1, expr)

makeTerm :: Rational -> Expr -> Expr
makeTerm coefficient body
    | coefficient == 0 = 0
    | coefficient == 1 = body
    | body == 1        = Number coefficient
    | otherwise        = Mul [Number coefficient, body]

makeProduct :: [Expr] -> Expr
makeProduct []  = 1
makeProduct [x] = x
makeProduct xs  = Mul xs

combineRepeatedFactorsRule :: Rule
combineRepeatedFactorsRule =
    productRule "combine-repeated-factors" $ \factors ->
        Just (Mul (combineAdjacent makePower factors))
  where
    makePower count factor =
        Pow factor (Number (fromIntegral count))

removeAddZeroRule :: Rule
removeAddZeroRule =
    sumRule "remove-add-zero" $
        Just . Add . filter (/= 0)

removeMulOneRule :: Rule
removeMulOneRule =
    productRule "remove-mul-one" $
        Just . Mul . filter (/= 1)

powerOneRule :: Rule
powerOneRule =
    rule "power-one" $ \expr ->
        case expr of
            Pow base (Number 1) -> Just base
            _                   -> Nothing

collapseAddRule :: Rule
collapseAddRule =
    sumRule "collapse-add" $ \terms ->
        case terms of
            []  -> Just 0
            [x] -> Just x
            _   -> Nothing

collapseMulRule :: Rule
collapseMulRule =
    productRule "collapse-mul" $ \factors ->
        case factors of
            []  -> Just 1
            [x] -> Just x
            _   -> Nothing

combineAdjacent :: Eq a => (Int -> a -> a) -> [a] -> [a]
combineAdjacent _ [] = []
combineAdjacent combine (x : xs) =
    let (same, rest) = span (== x) xs
        count        = 1 + length same
        first
            | count == 1 = x
            | otherwise  = combine count x
    in first : combineAdjacent combine rest

isNumber :: Expr -> Bool
isNumber (Number _) = True
isNumber _          = False
