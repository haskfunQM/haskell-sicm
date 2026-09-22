module Main (main) where

import SICM.Core.Expr (Expr, symbol)
import SICM.Math.Structure
    ( leaves
    , scalar
    , up
    )
import SICM.Mechanics.Lagrange
    ( dt
    , eulerLagrange
    )
import SICM.Mechanics.Local (localN)
import SICM.Symbolic.Differentiate (diff)
import SICM.Symbolic.Integrate (integrate)
import SICM.Symbolic.Polynomial
    ( polynomialNormalForm
    )
import SICM.Symbolic.Simplify (simplify)
import SICM.Symbolic.Proof (prove, (===))

canonical :: Expr -> Expr
canonical expression =
    case polynomialNormalForm (simplify expression) of
        Just result ->
            result

        Nothing ->
            simplify expression

main :: IO ()
main = do
    let t           = symbol "t"
        r           = symbol "r"
        theta       = symbol "theta"
        rdot        = symbol "rdot"
        thetadot    = symbol "thetadot"
        rddot       = symbol "rddot"
        thetaddot   = symbol "thetaddot"

        m           = symbol "m"
        e           = symbol "e"
        p           = symbol "p"
        a           = symbol "a"
        h           = symbol "h"
        mu          = symbol "mu"
        orbitalTime = symbol "T"

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

        acceleration =
            up
                [ scalar rddot
                , scalar thetaddot
                ]

        state =
            localN t q v [acceleration]

    putStrLn "============================================================"
    putStrLn "Kepler's laws -> inverse-square force"
    putStrLn "using haskell-sicm"
    putStrLn "============================================================"

    -- ------------------------------------------------------------------
    -- 1. Coordinate construction
    --
    -- The Sun is the polar origin:
    --
    --     x = r cos(theta)
    --     y = r sin(theta)
    --
    -- dt knows that q -> v -> acceleration from the Local state.
    -- We therefore no longer need a manually written 'rates' list.
    -- ------------------------------------------------------------------

    let x =
            r * cos theta

        y =
            r * sin theta

        xdot =
            dt state x

        ydot =
            dt state y

    putStrLn "\n1. Cartesian coordinates constructed from polar coordinates"
    putStrLn ("  x    = " ++ show x)
    putStrLn ("  y    = " ++ show y)
    putStrLn ("  xdot = " ++ show xdot)
    putStrLn ("  ydot = " ++ show ydot)

    -- Prove the polar-coordinate speed identity directly.
    --
    -- The prover now treats sin(theta) and cos(theta) as opaque algebraic
    -- atoms during expansion and knows sin^2(theta)+cos^2(theta)=1.
    let speedSquared =
            xdot ** 2 + ydot ** 2

        polarSpeedSquared =
            rdot ** 2 + r ** 2 * thetadot ** 2

    print $
        prove
            ( speedSquared
                === polarSpeedSquared
            )

    let kinetic =
            (1 / 2) * m * polarSpeedSquared

    putStrLn ("  v^2  = " ++ show polarSpeedSquared)
    putStrLn ("  T    = " ++ show kinetic)

    -- ------------------------------------------------------------------
    -- 2. Lagrange-d'Alembert before assuming gravity
    --
    -- For an unknown applied generalized force Q:
    --
    --     d/dt(partial T / partial qdot)
    --       - partial T / partial q
    --       = Q.
    --
    -- Our eulerLagrange function computes the left side.  With L=T, that
    -- left side is therefore the generalized force required by the orbit.
    -- ------------------------------------------------------------------

    let generalizedForce =
            eulerLagrange state kinetic

    (qRadial, qAngular) <-
        case leaves generalizedForce of
            [radialPart, angularPart] ->
                pure (radialPart, angularPart)

            _ ->
                error "expected two polar generalized-force components"

    putStrLn "\n2. Lagrange-d'Alembert generalized force"
    putStrLn ("  Q_r     = " ++ show (canonical qRadial))
    putStrLn ("  Q_theta = " ++ show (canonical qAngular))

    let hExpression =
            r ** 2 * thetadot

        hDot =
            dt state hExpression

    print $
        prove
            (qAngular === m * hDot)

    -- ------------------------------------------------------------------
    -- 3. Kepler II: equal areas in equal times
    --
    --     dA/dt = 1/2 r^2 thetadot = constant
    --
    -- so
    --
    --     h = r^2 thetadot = constant
    --     dh/dt = 0.
    --
    -- But Q_theta = m dh/dt, hence Q_theta=0.
    -- In polar coordinates Q_theta = r F_theta, so F_theta=0.
    -- Therefore the force is purely radial.
    -- ------------------------------------------------------------------

    putStrLn "\n3. Kepler II: the force has no tangential component"
    putStrLn "  dA/dt = (1/2) r^2 thetadot = constant"
    putStrLn "  h = r^2 thetadot = constant"
    putStrLn "  dh/dt = 0"
    putStrLn "  Q_theta = m dh/dt = 0"
    putStrLn "  Q_theta = r F_theta, therefore F_theta = 0"
    putStrLn "  => the force lies on the Sun-planet radial line"

    -- ------------------------------------------------------------------
    -- 4. Kepler I: ellipse with the Sun at a focus
    --
    --     r(theta) = p / (1 + e cos theta)
    --
    -- Let u = 1/r:
    --
    --     u(theta) = (1 + e cos theta)/p.
    --
    -- Differentiate the observed orbit geometry.
    -- ------------------------------------------------------------------

    let u =
            1 / p + (e / p) * cos theta

        uPrime =
            diff theta u

        uSecond =
            diff theta uPrime

        ellipseIdentity =
            simplify (uSecond + u)

    putStrLn "\n4. Kepler I: differentiate the ellipse"
    putStrLn ("  u        = " ++ show u)
    putStrLn ("  u'       = " ++ show uPrime)
    putStrLn ("  u''      = " ++ show uSecond)
    putStrLn ("  u'' + u  = " ++ show ellipseIdentity)
    putStrLn "  mathematically this reduces to 1/p"

    -- ------------------------------------------------------------------
    -- 5. Kepler I + II -> radial acceleration
    --
    -- From h = r^2 thetadot and u=1/r:
    --
    --     thetadot = h u^2
    --
    --     rdot = dr/dtheta * thetadot
    --          = -h u'
    --
    --     rddot = -h^2 u^2 u''
    --
    -- The radial component of acceleration in polar coordinates is
    --
    --     a_r = rddot - r thetadot^2
    --         = -h^2 u^2 (u'' + u).
    --
    -- Since u''+u=1/p and u=1/r:
    --
    --     a_r = -h^2 / (p r^2).
    -- ------------------------------------------------------------------

    let thetaDotFromAreaLaw =
            h * u ** 2

        rDotFromAreaLaw =
            -h * uPrime

        radialAccelerationInU =
            -h ** 2 * u ** 2 * (uSecond + u)

        radialAcceleration =
            -(h ** 2) / (p * r ** 2)

        radialForce =
            simplify (m * radialAcceleration)

    putStrLn "\n5. Ellipse + area law -> radial acceleration"
    putStrLn ("  thetadot = " ++ show thetaDotFromAreaLaw)
    putStrLn ("  rdot     = " ++ show rDotFromAreaLaw)
    putStrLn ("  a_r      = " ++ show radialAccelerationInU)
    putStrLn "  use u''+u = 1/p and u=1/r:"
    putStrLn ("  a_r      = " ++ show radialAcceleration)
    putStrLn ("  F_r      = " ++ show radialForce)
    putStrLn "  The minus sign means inward, toward the Sun."
    putStrLn "  The magnitude is proportional to 1/r^2."

    -- ------------------------------------------------------------------
    -- 6. Kepler III fixes the strength
    --
    -- Kepler II gives h=2A/T.  With ellipse area A=pi*a*b,
    -- b^2=a^2(1-e^2), and p=a(1-e^2):
    --
    --     h^2/p = 4 pi^2 a^3/T^2.
    --
    -- Kepler III says T^2/a^3 is the same for all planets around the
    -- same Sun.  Therefore h^2/p is one common constant.  Call it mu.
    -- ------------------------------------------------------------------

    let muFromKepler3 =
            4 * pi ** 2 * a ** 3 / orbitalTime ** 2

        inverseSquareForce =
            -m * mu / r ** 2

    putStrLn "\n6. Kepler III: common force strength"
    putStrLn ("  mu = " ++ show muFromKepler3)
    putStrLn "  Kepler III makes this common to all planets around the Sun."
    putStrLn ("  F_r = " ++ show inverseSquareForce)

    -- ------------------------------------------------------------------
    -- 7. Recover the conservative potential
    --
    --     F_r = -dV/dr
    --
    -- therefore
    --
    --     dV/dr = m mu / r^2.
    --
    -- Let our symbolic integrator recover V.
    -- ------------------------------------------------------------------

    let dVdr =
            m * mu * r ** (-2)

        potential =
            case integrate r dVdr of
                Just result ->
                    result

                Nothing ->
                    error "symbolic integration of dV/dr failed"

        gravitationalLagrangian =
            simplify (kinetic - potential)

    putStrLn "\n7. Recover potential and gravitational Lagrangian"
    putStrLn ("  dV/dr = " ++ show dVdr)
    putStrLn ("  V(r)  = " ++ show potential)
    putStrLn ("  L     = " ++ show gravitationalLagrangian)

    -- ------------------------------------------------------------------
    -- 8. Close the loop with the ordinary Euler-Lagrange equation
    --
    -- Now gravity is encoded in V, so eulerLagrange L = 0 gives the
    -- equations of motion directly.
    -- ------------------------------------------------------------------

    let finalResidual =
            eulerLagrange state gravitationalLagrangian

    putStrLn "\n8. Euler-Lagrange verification"
    putStrLn ("  E(L) = " ++ show finalResidual)
    putStrLn "  Set every component of E(L) equal to zero."

    putStrLn "\nResult"
    putStrLn "  F_theta = 0"
    putStrLn "  F_r = -m mu/r^2"
    putStrLn "  => force points toward the Sun"
    putStrLn "  => |F| is proportional to 1/r^2"
    putStrLn ""
    putStrLn "Newton's universal-gravity step identifies mu = G M_sun."
