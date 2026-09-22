module SICM.Symbolic.Rewrite
    ( Rule
    , rule
    , ruleName
    , applyRule
    , applyFirst
    , firstOf
    , sumRule
    , productRule
    , rewriteOnce
    , rewriteRepeatedly
    , rewriteRules
    ) where

import SICM.Core.Expr (Expr(..))

data Rule = Rule
    { ruleName :: String
    , runRule  :: Expr -> Maybe Expr
    }

rule :: String -> (Expr -> Maybe Expr) -> Rule
rule = Rule

sumRule :: String -> ([Expr] -> Maybe Expr) -> Rule
sumRule name transform =
    rule name $ \expr ->
        case expr of
            Add terms -> transform terms
            _         -> Nothing

productRule :: String -> ([Expr] -> Maybe Expr) -> Rule
productRule name transform =
    rule name $ \expr ->
        case expr of
            Mul factors -> transform factors
            _           -> Nothing

applyRule :: Rule -> Expr -> Maybe Expr
applyRule rewriteRule expr =
    case runRule rewriteRule expr of
        Just result
            | result /= expr -> Just result
        _ -> Nothing

applyFirst :: [Rule] -> Expr -> Maybe (Rule, Expr)
applyFirst [] _ = Nothing
applyFirst (rewriteRule : rules) expr =
    case applyRule rewriteRule expr of
        Just result -> Just (rewriteRule, result)
        Nothing     -> applyFirst rules expr

firstOf :: [Rule] -> Rule
firstOf rules =
    rule "first-applicable" $ \expr ->
        snd <$> applyFirst rules expr

rewriteOnce :: Rule -> Expr -> Maybe Expr
rewriteOnce rewriteRule expr =
    case applyRule rewriteRule expr of
        Just result -> Just result
        Nothing     -> rewriteChild expr
  where
    rewriteChild current =
        case current of
            Number _ -> Nothing
            Symbol _ -> Nothing

            Add terms ->
                Add <$> rewriteFirst (rewriteOnce rewriteRule) terms

            Mul factors ->
                Mul <$> rewriteFirst (rewriteOnce rewriteRule) factors

            Pow base power ->
                case rewriteOnce rewriteRule base of
                    Just base' -> Just (Pow base' power)
                    Nothing    -> Pow base <$> rewriteOnce rewriteRule power

            Apply name arguments ->
                Apply name <$> rewriteFirst (rewriteOnce rewriteRule) arguments

-- | Repeatedly rewrite until no rule applies.
--
-- A rewrite system should move toward a normal form.  If it returns to a
-- previously seen expression, the rules contain a cycle; report that bug
-- instead of looping forever.
rewriteRepeatedly :: Rule -> Expr -> Expr
rewriteRepeatedly rewriteRule =
    go []
  where
    go seen expr =
        case rewriteOnce rewriteRule expr of
            Nothing ->
                expr

            Just result
                | result `elem` seen ->
                    error
                        ("rewriteRepeatedly: rewrite cycle detected in "
                            ++ ruleName rewriteRule)

                | otherwise ->
                    go (expr : seen) result

rewriteRules :: [Rule] -> Expr -> Expr
rewriteRules rules =
    rewriteRepeatedly (firstOf rules)

rewriteFirst :: (a -> Maybe a) -> [a] -> Maybe [a]
rewriteFirst _ [] = Nothing
rewriteFirst transform (x : xs) =
    case transform x of
        Just x' -> Just (x' : xs)
        Nothing -> (x :) <$> rewriteFirst transform xs
