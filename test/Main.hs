module Main (main) where

import Control.Monad (unless)

import SICM.Core.Expr (Expr(..), symbol)
import SICM.Math.Structure
    ( components
    , down
    , dual
    , leaves
    , sameShape
    , scalar
    , up
    , zipWithStructure
    )
import SICM.Mechanics.Lagrange (dt, eulerLagrange, partial)
import SICM.Mechanics.Local
    ( acceleration
    , coordinates
    , derivative
    , gamma
    , gammaN
    , local
    , localN
    , time
    , velocity
    )
import SICM.Symbolic.Function (literalFunction)
import SICM.Symbolic.Polynomial (polynomialEquivalent)
import SICM.Symbolic.Proof (isProven, prove, (===))

main :: IO ()
main = do
    functionTests
    structureTests
    localTests
    generalizedLocalTests
    lagrangeTests
    totalDerivativeTests
    eulerLagrangeTests
    keplerCoreTests
    proverIdentityTests
    putStrLn "All tests passed."

functionTests :: IO ()
functionTests = do
    let t = symbol "t"

        q = literalFunction "q"
        u = literalFunction "U"

        squarePlusOne x =
            x * x + 1

        combined x =
            q x + u x

    assertEqual
        "literal function application"
        (Apply "q" [t])
        (q t)

    assertEqual
        "ordinary Haskell composition"
        (Apply "U" [Apply "q" [t]])
        ((u . q) t)

    assertEqual
        "ordinary Haskell function"
        (t * t + 1)
        (squarePlusOne t)

    assertEqual
        "ordinary Haskell function combining symbolic functions"
        (Add [Apply "q" [t], Apply "U" [t]])
        (combined t)

structureTests :: IO ()
structureTests = do
    let t  = symbol "t"
        x  = symbol "x"
        y  = symbol "y"
        vx = symbol "vx"
        vy = symbol "vy"

        position =
            up
                [ scalar x
                , scalar y
                ]

        velocityState =
            up
                [ scalar vx
                , scalar vy
                ]

        state =
            up
                [ scalar t
                , position
                , velocityState
                ]

        covector =
            down
                [ scalar x
                , scalar y
                ]

    assertEqual
        "up structure display"
        "up(x, y)"
        (show position)

    assertEqual
        "nested structure display"
        "up(t, up(x, y), up(vx, vy))"
        (show state)

    assertEqual
        "down structure display"
        "down(x, y)"
        (show covector)

    assertEqual
        "Functor maps scalar leaves"
        ( up
            [ scalar (x + 1)
            , scalar (y + 1)
            ]
        )
        (fmap (+ 1) position)

    assertEqual
        "leaves preserve left-to-right order"
        [t, x, y, vx, vy]
        (leaves state)

    assertEqual
        "sameShape ignores scalar values"
        True
        ( sameShape
            position
            (up [scalar t, scalar vx])
        )

    assertEqual
        "sameShape distinguishes orientation"
        False
        (sameShape position covector)

    assertEqual
        "components exposes one level only"
        (Just [scalar x, scalar y])
        (components position)


    assertEqual
        "dual reverses orientation recursively"
        ( down
            [ scalar x
            , down
                [ scalar y
                , scalar vx
                ]
            ]
        )
        ( dual
            ( up
                [ scalar x
                , up
                    [ scalar y
                    , scalar vx
                    ]
                ]
            )
        )

    assertEqual
        "dual is an involution"
        position
        (dual (dual position))

localTests :: IO ()
localTests = do
    let t  = symbol "t"
        x  = symbol "x"
        y  = symbol "y"
        vx = symbol "vx"
        vy = symbol "vy"

        explicitPosition =
            up
                [ scalar x
                , scalar y
                ]

        explicitVelocity =
            up
                [ scalar vx
                , scalar vy
                ]

        explicitLocal =
            local t explicitPosition explicitVelocity

        path s =
            up
                [ scalar (s ** 2)
                , scalar (sin s)
                ]

        lifted =
            gamma path t

    assertEqual
        "local time"
        t
        (time explicitLocal)

    assertEqual
        "local coordinates"
        explicitPosition
        (coordinates explicitLocal)

    assertEqual
        "local velocity"
        explicitVelocity
        (velocity explicitLocal)

    assertEqual
        "gamma coordinates"
        ( up
            [ scalar (t ** 2)
            , scalar (sin t)
            ]
        )
        (coordinates lifted)

    assertEqual
        "gamma differentiates every coordinate"
        ( up
            [ scalar (2 * t)
            , scalar (cos t)
            ]
        )
        (velocity lifted)

    assertEqual
        "gamma preserves time"
        t
        (time lifted)



generalizedLocalTests :: IO ()
generalizedLocalTests = do
    let t =
            symbol "t"

        path s =
            up
                [ scalar (s ** 3)
                , scalar (sin s)
                ]

        firstOrder =
            gamma path t

        secondOrder =
            gammaN 2 path t

        thirdOrder =
            gammaN 3 path t

    assertEqual
        "gamma remains first-order"
        Nothing
        (acceleration firstOrder)

    assertEqual
        "gammaN 2 carries acceleration"
        ( Just
            ( up
                [ scalar (6 * t)
                , scalar (-sin t)
                ]
            )
        )
        (acceleration secondOrder)

    assertEqual
        "derivative 0 selects coordinates"
        (Just (coordinates thirdOrder))
        (derivative 0 thirdOrder)

    assertEqual
        "derivative 1 selects velocity"
        (Just (velocity thirdOrder))
        (derivative 1 thirdOrder)

    assertEqual
        "gammaN 3 carries third derivative"
        ( Just
            ( up
                [ scalar 6
                , scalar (-cos t)
                ]
            )
        )
        (derivative 3 thirdOrder)

    assertEqual
        "missing derivative level returns Nothing"
        Nothing
        (derivative 4 thirdOrder)

lagrangeTests :: IO ()
lagrangeTests = do
    let x  = symbol "x"
        y  = symbol "y"
        vx = symbol "vx"
        vy = symbol "vy"

        q =
            up
                [ scalar x
                , scalar y
                ]

        v =
            up
                [ scalar vx
                , scalar vy
                ]

        positionExpression =
            x ** 2 + 3 * y

        velocityExpression =
            vx ** 2 + 3 * vy

        nestedVariables =
            up
                [ scalar x
                , up
                    [ scalar y
                    , scalar vx
                    ]
                ]

        nestedExpression =
            x ** 2 + y ** 2 + vx ** 2

    assertEqual
        "structured partial derivative with respect to coordinates"
        ( down
            [ scalar (2 * x)
            , scalar 3
            ]
        )
        (partial q positionExpression)

    assertEqual
        "structured partial derivative with respect to velocity"
        ( down
            [ scalar (2 * vx)
            , scalar 3
            ]
        )
        (partial v velocityExpression)

    assertEqual
        "scalar partial derivative stays scalar"
        (scalar (3 * (x ** 2)))
        (partial (scalar x) (x ** 3))

    assertEqual
        "partial reverses nested orientations"
        ( down
            [ scalar (2 * x)
            , down
                [ scalar (2 * y)
                , scalar (2 * vx)
                ]
            ]
        )
        (partial nestedVariables nestedExpression)


totalDerivativeTests :: IO ()
totalDerivativeTests = do
    let t          = symbol "t"
        r          = symbol "r"
        theta      = symbol "theta"
        rdot       = symbol "rdot"
        thetadot   = symbol "thetadot"
        rddot      = symbol "rddot"
        thetaddot  = symbol "thetaddot"

        q =
            up
                [ scalar r
                , scalar theta
                ]

        v =
            up
                [ scalar rdot
                , scalar thetadot
                ]

        a =
            up
                [ scalar rddot
                , scalar thetaddot
                ]

        state =
            localN t q v [a]

        areaExpression =
            r ** 2 * thetadot

        expectedAreaDerivative =
            2 * r * rdot * thetadot
                + r ** 2 * thetaddot

        explicitTimeExpression =
            t * r

        expectedExplicitTimeDerivative =
            r + t * rdot

    assertEqual
        "localN carries explicit acceleration"
        (Just a)
        (acceleration state)

    assertEqual
        "dt maps r to rdot"
        rdot
        (dt state r)

    assertEqual
        "dt maps rdot to rddot"
        rddot
        (dt state rdot)

    assertEqual
        "dt implements the chain rule for r^2 thetadot"
        (Just True)
        ( polynomialEquivalent
            (dt state areaExpression)
            expectedAreaDerivative
        )

    assertEqual
        "dt includes explicit time dependence"
        (Just True)
        ( polynomialEquivalent
            (dt state explicitTimeExpression)
            expectedExplicitTimeDerivative
        )


eulerLagrangeTests :: IO ()
eulerLagrangeTests = do
    let t  = symbol "t"
        x  = symbol "x"
        y  = symbol "y"
        vx = symbol "vx"
        vy = symbol "vy"
        ax = symbol "ax"
        ay = symbol "ay"
        m  = symbol "m"
        k  = symbol "k"

        q1 =
            scalar x

        v1 =
            scalar vx

        a1 =
            scalar ax

        state1 =
            localN t q1 v1 [a1]

        harmonicLagrangian =
            (1 / 2) * m * vx ** 2
                - (1 / 2) * k * x ** 2

        harmonicResidual =
            eulerLagrange state1 harmonicLagrangian

        q2 =
            up
                [ scalar x
                , scalar y
                ]

        v2 =
            up
                [ scalar vx
                , scalar vy
                ]

        a2 =
            up
                [ scalar ax
                , scalar ay
                ]

        state2 =
            localN t q2 v2 [a2]

        oscillator2D =
            (1 / 2) * m * (vx ** 2 + vy ** 2)
                - (1 / 2) * k * (x ** 2 + y ** 2)

        residual2D =
            eulerLagrange state2 oscillator2D

        expected2D =
            down
                [ scalar (m * ax + k * x)
                , scalar (m * ay + k * y)
                ]

    assertEqual
        "zipWithStructure combines matching structures"
        ( Just
            ( up
                [ scalar (x + vx)
                , scalar (y + vy)
                ]
            )
        )
        (zipWithStructure (+) q2 v2)

    assertEqual
        "zipWithStructure rejects opposite orientations"
        Nothing
        (zipWithStructure (+) q2 (down [scalar vx, scalar vy]))

    assertEqual
        "Euler-Lagrange residual has dual orientation"
        True
        (sameShape residual2D expected2D)

    case leaves harmonicResidual of
        [residual] ->
            assertEqual
                "harmonic oscillator Euler-Lagrange equation"
                (Just True)
                (polynomialEquivalent residual (m * ax + k * x))

        _ ->
            error "harmonic oscillator residual should contain one equation"

    case (leaves residual2D, leaves expected2D) of
        ([rx, ry], [ex, ey]) -> do
            assertEqual
                "2D oscillator Euler-Lagrange x equation"
                (Just True)
                (polynomialEquivalent rx ex)

            assertEqual
                "2D oscillator Euler-Lagrange y equation"
                (Just True)
                (polynomialEquivalent ry ey)

        _ ->
            error "2D oscillator residual should contain two equations"


keplerCoreTests :: IO ()
keplerCoreTests = do
    let t          = symbol "t"
        r          = symbol "r"
        theta      = symbol "theta"
        rdot       = symbol "rdot"
        thetadot   = symbol "thetadot"
        rddot      = symbol "rddot"
        thetaddot  = symbol "thetaddot"
        m          = symbol "m"

        q =
            up
                [ scalar r
                , scalar theta
                ]

        v =
            up
                [ scalar rdot
                , scalar thetadot
                ]

        a =
            up
                [ scalar rddot
                , scalar thetaddot
                ]

        state =
            localN t q v [a]

        kinetic =
            (1 / 2) * m
                * (rdot ** 2 + r ** 2 * thetadot ** 2)

        residual =
            eulerLagrange state kinetic

        expectedRadial =
            m * (rddot - r * thetadot ** 2)

        expectedAngular =
            m
                * ( 2 * r * rdot * thetadot
                  + r ** 2 * thetaddot
                  )

        areaMomentum =
            r ** 2 * thetadot

    case leaves residual of
        [radialPart, angularPart] -> do
            assertEqual
                "polar radial Lagrange-d'Alembert component"
                (Just True)
                (polynomialEquivalent radialPart expectedRadial)

            assertEqual
                "polar angular Lagrange-d'Alembert component"
                (Just True)
                (polynomialEquivalent angularPart expectedAngular)

            assertEqual
                "Kepler area momentum matches angular component"
                (Just True)
                ( polynomialEquivalent
                    angularPart
                    (m * dt state areaMomentum)
                )

        _ ->
            error "polar Euler-Lagrange result should contain two equations"


proverIdentityTests :: IO ()
proverIdentityTests = do
    let x        = symbol "x"
        y        = symbol "y"
        r        = symbol "r"
        theta    = symbol "theta"
        rdot     = symbol "rdot"
        thetadot = symbol "thetadot"
        rddot    = symbol "rddot"
        thetaddot = symbol "thetaddot"
        t        = symbol "t"

        q =
            up
                [ scalar r
                , scalar theta
                ]

        v =
            up
                [ scalar rdot
                , scalar thetadot
                ]

        a =
            up
                [ scalar rddot
                , scalar thetaddot
                ]

        state =
            localN t q v [a]

        xCartesian =
            r * cos theta

        yCartesian =
            r * sin theta

        xdot =
            dt state xCartesian

        ydot =
            dt state yCartesian

        polarSpeedSquared =
            rdot ** 2 + r ** 2 * thetadot ** 2

        proven equation =
            isProven (prove equation)

    assertEqual
        "prove sin^2+cos^2=1"
        True
        (proven (sin x ** 2 + cos x ** 2 === 1))

    assertEqual
        "prove cos^2+sin^2=1"
        True
        (proven (cos x ** 2 + sin x ** 2 === 1))

    assertEqual
        "prove sin(-x)=-sin(x)"
        True
        (proven (sin (-x) === -sin x))

    assertEqual
        "prove cos(-x)=cos(x)"
        True
        (proven (cos (-x) === cos x))

    assertEqual
        "prove sin(0)=0"
        True
        (proven (sin (0 :: Expr) === (0 :: Expr)))

    assertEqual
        "prove cos(0)=1"
        True
        (proven (cos (0 :: Expr) === (1 :: Expr)))

    assertEqual
        "prove sin(pi/2)=1"
        True
        (proven (sin ((pi / 2) :: Expr) === 1))

    assertEqual
        "prove cos(pi/2)=0"
        True
        (proven (cos ((pi / 2) :: Expr) === 0))

    assertEqual
        "prove sin(pi)=0"
        True
        (proven (sin (pi :: Expr) === 0))

    assertEqual
        "prove cos(pi)=-1"
        True
        (proven (cos (pi :: Expr) === -1))

    assertEqual
        "prove sin addition"
        True
        ( proven
            ( sin (x + y)
                === sin x * cos y + cos x * sin y
            )
        )

    assertEqual
        "prove cos addition"
        True
        ( proven
            ( cos (x + y)
                === cos x * cos y - sin x * sin y
            )
        )

    assertEqual
        "prove sin double angle"
        True
        (proven (sin (2 * x) === 2 * sin x * cos x))

    assertEqual
        "prove cos double angle"
        True
        (proven (cos (2 * x) === cos x ** 2 - sin x ** 2))

    assertEqual
        "prove exp(0)=1"
        True
        (proven (exp (0 :: Expr) === (1 :: Expr)))

    assertEqual
        "prove log(exp(x))=x"
        True
        (proven (log (exp x) === x))

    assertEqual
        "prove polar speed directly without c/s placeholders"
        True
        ( proven
            ( xdot ** 2 + ydot ** 2
                === polarSpeedSquared
            )
        )

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
    unless (expected == actual) $
        error
            ( label
                ++ "\n  expected: "
                ++ show expected
                ++ "\n  but got:  "
                ++ show actual
            )
