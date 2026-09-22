module SICM.Mechanics.Lagrange
    ( partial
    , dt
    , eulerLagrange
    ) where

import SICM.Core.Expr (Expr)
import SICM.Math.Structure
    ( Structure
    , dual
    , leaves
    , sameShape
    , zipWithStructure
    )
import SICM.Mechanics.Local
    ( Local
    , coordinates
    , derivative
    , time
    , velocity
    )
import SICM.Symbolic.Differentiate
    ( dependsOn
    , diff
    )
import SICM.Symbolic.Simplify (simplify)

-- | Differentiate a scalar expression with respect to every scalar component
-- of a structured variable.
--
-- The derivative has the dual orientation of the variable structure.
partial :: Structure Expr -> Expr -> Structure Expr
partial variables expression =
    dual (fmap differentiate variables)
  where
    differentiate variable =
        diff variable expression

-- | Total time derivative in local coordinates.
--
-- For a local state
--
--     (t, q, Dq, D^2 q, ...)
--
-- this implements
--
--     D_t F
--       = partial F / partial t
--       + partial F / partial q     . Dq
--       + partial F / partial Dq    . D^2 q
--       + ...
--
-- The Local value must contain one derivative level beyond the highest level
-- on which the expression depends.  For example, differentiating a function
-- of q and Dq requires a Local state containing D^2 q.
dt :: Local Expr -> Expr -> Expr
dt state expression
    | dependsOnHighestLevel =
        error
            "dt: local state needs one more derivative level for this expression"
    | otherwise =
        simplify
            ( diff (time state) expression
            + sum derivativeTerms
            )
  where
    levels =
        localLevels state

    dependsOnHighestLevel =
        case reverse levels of
            [] ->
                False

            highest : _ ->
                any
                    (\variable -> dependsOn variable expression)
                    (leaves highest)

    derivativeTerms =
        concatMap pairTerms (adjacentPairs levels)

    pairTerms (variables, rates)
        | sameShape variables rates =
            zipWith
                (\variable rate -> diff variable expression * rate)
                (leaves variables)
                (leaves rates)

        | otherwise =
            error "dt: adjacent derivative levels have different shapes"

localLevels :: Local a -> [Structure a]
localLevels state =
    go 0
  where
    go order =
        case derivative order state of
            Nothing ->
                []

            Just level ->
                level : go (order + 1)

adjacentPairs :: [a] -> [(a, a)]
adjacentPairs values =
    zip values (drop 1 values)


-- | Euler-Lagrange residual for a scalar Lagrangian evaluated on a symbolic
-- local state:
--
--     E(L) = D_t (partial L / partial v)
--          -       partial L / partial q
--
-- The result has the dual orientation of the coordinate structure.  The
-- equations of motion are obtained by setting every scalar component of the
-- returned structure equal to zero.
--
-- The supplied Local state must carry acceleration, because D_t(partial L /
-- partial v) generally depends on D^2 q.
eulerLagrange :: Local Expr -> Expr -> Structure Expr
eulerLagrange state lagrangian =
    case zipWithStructure subtractExpr momentumRate coordinateDerivative of
        Just result ->
            fmap simplify result

        Nothing ->
            error
                "eulerLagrange: coordinate and velocity structures have incompatible shapes"
  where
    coordinateDerivative =
        partial (coordinates state) lagrangian

    momentumDerivative =
        partial (velocity state) lagrangian

    momentumRate =
        fmap (dt state) momentumDerivative

    subtractExpr left right =
        left - right
