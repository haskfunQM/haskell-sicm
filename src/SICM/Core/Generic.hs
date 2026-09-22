module SICM.Core.Generic
    ( square
    , cube
    , reciprocal
    ) where

-- | Square a mathematical object using its multiplication operation.
--
-- This is intentionally generic. It works for ordinary numbers now,
-- for symbolic expressions through their 'Num' instance, and later for
-- any mathematical type for which multiplication has a sensible meaning.
square :: Num a => a -> a
square x = x * x

-- | Cube a mathematical object.
cube :: Num a => a -> a
cube x = x * x * x

-- | Multiplicative reciprocal.
--
-- This is just 'recip' with a name that reads naturally in mathematical
-- code. For symbolic expressions it constructs a symbolic inverse.
reciprocal :: Fractional a => a -> a
reciprocal = recip
