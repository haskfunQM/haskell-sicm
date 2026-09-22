module SICM.Mechanics.Local
    ( Local
    , local
    , localN
    , time
    , coordinates
    , velocity
    , acceleration
    , derivative
    , gamma
    , gammaN
    ) where

import Data.List (intercalate)

import SICM.Core.Expr (Expr)
import SICM.Math.Structure (Structure)
import SICM.Symbolic.Differentiate (diff)

-- | A local mechanical state.
--
-- A first-order local state contains
--
--     (t, q, Dq)
--
-- and may also carry higher derivatives:
--
--     (t, q, Dq, D^2 q, D^3 q, ...)
data Local a =
    Local
        a
        (Structure a)
        (Structure a)
        [Structure a]
    deriving (Eq)

instance Show a => Show (Local a) where
    show (Local t q v higher) =
        "local("
            ++ intercalate ", " (show t : map show (q : v : higher))
            ++ ")"

-- | Construct the ordinary first-order local state (t, q, Dq).
local :: a -> Structure a -> Structure a -> Local a
local t q v =
    Local t q v []

-- | Construct a local state with explicitly supplied higher derivatives.
--
-- The list begins with acceleration:
--
--     localN t q v [a]       = (t, q, v, a)
--     localN t q v [a, jerk] = (t, q, v, a, jerk)
--
-- This is useful when q, v, a, ... are independent symbolic local
-- coordinates rather than expressions obtained from a concrete trajectory.
localN :: a -> Structure a -> Structure a -> [Structure a] -> Local a
localN =
    Local

time :: Local a -> a
time (Local t _ _ _) =
    t

coordinates :: Local a -> Structure a
coordinates (Local _ q _ _) =
    q

velocity :: Local a -> Structure a
velocity (Local _ _ v _) =
    v

-- | Acceleration D^2 q, when present.
acceleration :: Local a -> Maybe (Structure a)
acceleration =
    derivative 2

-- | Select a derivative level:
--
--     0 -> q
--     1 -> Dq
--     2 -> D^2 q
--     ...
derivative :: Int -> Local a -> Maybe (Structure a)
derivative order (Local _ q v higher)
    | order < 0 =
        Nothing
    | order == 0 =
        Just q
    | order == 1 =
        Just v
    | otherwise =
        atIndex (order - 2) higher

-- | Ordinary first-order lift:
--
--     Gamma[q](t) = (t, q(t), Dq(t)).
gamma :: (Expr -> Structure Expr) -> Expr -> Local Expr
gamma =
    gammaN 1

-- | Lift a trajectory through a requested derivative order:
--
--     gammaN 1 q t = (t, q, Dq)
--     gammaN 2 q t = (t, q, Dq, D^2 q)
--     gammaN 3 q t = (t, q, Dq, D^2 q, D^3 q)
--
-- The requested order must be at least 1.
gammaN :: Int -> (Expr -> Structure Expr) -> Expr -> Local Expr
gammaN order path t
    | order < 1 =
        error "gammaN: derivative order must be at least 1"
    | otherwise =
        Local t q0 q1 higher
  where
    differentiate =
        fmap (diff t)

    q0 =
        path t

    q1 =
        differentiate q0

    higher =
        take (order - 1)
            (drop 1 (iterate differentiate q1))

atIndex :: Int -> [a] -> Maybe a
atIndex index values
    | index < 0 =
        Nothing
    | otherwise =
        go index values
  where
    go _ [] =
        Nothing
    go 0 (value : _) =
        Just value
    go n (_ : rest) =
        go (n - 1) rest
