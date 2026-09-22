module SICM.Symbolic.Proof
    ( Equation
    , (===)
    , Provable(..)
    , Proof
    , ProofStatus(..)
    , proposition
    , residual
    , status
    , steps
    , isProven
    ) where

import Data.Maybe (fromMaybe)

import SICM.Core.Expr (Expr)
import SICM.Symbolic.Identity
    ( expandIdentities
    , reduceIdentities
    )
import SICM.Symbolic.Polynomial
    ( algebraicEquivalent
    , algebraicNormalForm
    , polynomialEquivalent
    , polynomialExpansion
    )
import SICM.Symbolic.Simplify (simplify)

data Equation a =
    Equation a a
    deriving (Eq)

infix 4 ===

(===) :: a -> a -> Equation a
(===) =
    Equation

class Provable a where
    prove :: Equation a -> Proof a
    renderValue :: a -> String

instance Provable a => Show (Equation a) where
    show (Equation left right) =
        renderValue left ++ " = " ++ renderValue right

data ProofStatus
    = Proven
    | Unresolved
    deriving (Eq, Show)

data Proof a = Proof
    { proposition :: Equation a
    , residual    :: Expr
    , status      :: ProofStatus
    , steps       :: [String]
    }
    deriving (Eq)

instance Provable a => Show (Proof a) where
    show proof =
        case status proof of
            Proven
                | null (steps proof) ->
                    "Proven: " ++ show (proposition proof)

                | otherwise ->
                    "Prove: "
                        ++ show (proposition proof)
                        ++ "\n"
                        ++ unlines (map ("  " ++) (steps proof))
                        ++ "Proven."

            Unresolved ->
                "Unresolved: "
                    ++ show (proposition proof)
                    ++ "\nResidual: "
                    ++ show (residual proof)

instance Provable Expr where
    prove =
        proveExpr

    renderValue =
        show

proveExpr :: Equation Expr -> Proof Expr
proveExpr equation@(Equation left right) =
    let leftNormal =
            simplify left

        rightNormal =
            simplify right
    in
        if leftNormal == rightNormal
            then
                proven equation

            else
                case polynomialEquivalent leftNormal rightNormal of
                    Just True ->
                        provenWithSteps
                            equation
                            (expansionSteps leftNormal rightNormal)

                    _ ->
                        case proveByIdentities leftNormal rightNormal of
                            Just identityProofSteps ->
                                provenWithSteps equation identityProofSteps

                            Nothing ->
                                proveExprByResidual
                                    equation
                                    leftNormal
                                    rightNormal

-- | Try the small library of common mathematical identities.
--
-- The sequence is intentional:
--
--   1. expand named trig identities when present;
--   2. perform algebraic expansion while treating functions as opaque atoms;
--   3. reduce canonical identities such as cos^2 = 1-sin^2;
--   4. canonicalize algebraically once more.
--
-- This is enough, for example, to prove the polar-coordinate speed identity
-- directly with sin(theta) and cos(theta), without introducing temporary
-- symbols c and s.
proveByIdentities :: Expr -> Expr -> Maybe [String]
proveByIdentities left right =
    let leftIdentity =
            identityCanonical left

        rightIdentity =
            identityCanonical right
    in
        if leftIdentity == rightIdentity
            then
                Just (identitySteps left right leftIdentity rightIdentity)

            else
                case algebraicEquivalent leftIdentity rightIdentity of
                    Just True ->
                        Just (identitySteps left right leftIdentity rightIdentity)

                    _ ->
                        Nothing

identityCanonical :: Expr -> Expr
identityCanonical expression =
    let identityExpanded =
            expandIdentities expression

        algebraExpanded =
            fromMaybe
                identityExpanded
                (algebraicNormalForm identityExpanded)

        identityReduced =
            reduceIdentities algebraExpanded

        simplified =
            simplify identityReduced
    in
        fromMaybe
            simplified
            (algebraicNormalForm simplified)

identitySteps :: Expr -> Expr -> Expr -> Expr -> [String]
identitySteps left right leftResult rightResult =
    leftStep ++ rightStep
  where
    leftStep
        | leftResult == left =
            []

        | otherwise =
            [ show left
                ++ " = "
                ++ show leftResult
                ++ "    [identity/algebra]"
            ]

    rightStep
        | rightResult == right =
            []

        | otherwise =
            [ show right
                ++ " = "
                ++ show rightResult
                ++ "    [identity/algebra]"
            ]

proveExprByResidual :: Equation Expr -> Expr -> Expr -> Proof Expr
proveExprByResidual equation left right =
    let reduced =
            simplify (left - right)
    in
        if reduced == 0
            then
                proven equation

            else
                Proof
                    { proposition = equation
                    , residual = reduced
                    , status = Unresolved
                    , steps = []
                    }

proven :: Equation a -> Proof a
proven equation =
    provenWithSteps equation []

provenWithSteps :: Equation a -> [String] -> Proof a
provenWithSteps equation proofSteps =
    Proof
        { proposition = equation
        , residual = 0
        , status = Proven
        , steps = proofSteps
        }

-- Only genuine polynomial expansion is narrated.  Canonical reordering such
-- as x+y versus y+x is silent normalization.
expansionSteps :: Expr -> Expr -> [String]
expansionSteps left right =
    leftStep ++ rightStep
  where
    leftStep =
        case polynomialExpansion left of
            Just expanded ->
                [ show left
                    ++ " = "
                    ++ show expanded
                    ++ "    [expand]"
                ]

            Nothing ->
                []

    rightStep =
        case polynomialExpansion right of
            Just expanded ->
                [ show right
                    ++ " = "
                    ++ show expanded
                    ++ "    [expand]"
                ]

            Nothing ->
                []

isProven :: Proof a -> Bool
isProven proof =
    status proof == Proven
