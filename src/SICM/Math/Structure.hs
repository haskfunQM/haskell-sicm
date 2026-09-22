module SICM.Math.Structure
    ( Structure
    , scalar
    , up
    , down
    , dual
    , components
    , leaves
    , sameShape
    , zipWithStructure
    ) where

import Data.List (intercalate)

-- | A recursively nested mathematical structure.
--
-- SICM uses oriented structures rather than treating every collection as a
-- vector.  A local state can contain a scalar time together with structured
-- coordinates and velocities, for example:
--
--     up
--         [ scalar t
--         , up [scalar x, scalar y]
--         , up [scalar vx, scalar vy]
--         ]
--
-- The constructors stay private.  Users build structures with 'scalar',
-- 'up', and 'down'.
data Structure a
    = Scalar a
    | Upward [Structure a]
    | Downward [Structure a]
    deriving (Eq)

instance Show a => Show (Structure a) where
    show structure =
        case structure of
            Scalar value ->
                show value

            Upward values ->
                "up(" ++ showComponents values ++ ")"

            Downward values ->
                "down(" ++ showComponents values ++ ")"
      where
        showComponents =
            intercalate ", " . map show

-- | Embed an ordinary value as a scalar structure.
scalar :: a -> Structure a
scalar =
    Scalar

-- | Construct an upward-oriented structure.
up :: [Structure a] -> Structure a
up =
    Upward

-- | Construct a downward-oriented structure.
down :: [Structure a] -> Structure a
down =
    Downward

-- | Reverse the orientation of every structured level.
--
-- Scalars are unchanged:
--
--     dual (up   [...]) = down [...]
--     dual (down [...]) = up   [...]
--
-- Applying 'dual' twice restores the original structure.
dual :: Structure a -> Structure a
dual structure =
    case structure of
        Scalar value ->
            Scalar value

        Upward values ->
            Downward (map dual values)

        Downward values ->
            Upward (map dual values)

-- | Return the immediate components of an oriented structure.
--
-- A scalar has no components.
components :: Structure a -> Maybe [Structure a]
components structure =
    case structure of
        Scalar _ ->
            Nothing

        Upward values ->
            Just values

        Downward values ->
            Just values

-- | Collect all scalar leaves from left to right.
leaves :: Structure a -> [a]
leaves structure =
    case structure of
        Scalar value ->
            [value]

        Upward values ->
            concatMap leaves values

        Downward values ->
            concatMap leaves values

-- | Test whether two structures have the same recursive orientation and
-- shape, ignoring their scalar values.
sameShape :: Structure a -> Structure b -> Bool
sameShape left right =
    case (left, right) of
        (Scalar _, Scalar _) ->
            True

        (Upward leftValues, Upward rightValues) ->
            sameComponentShapes leftValues rightValues

        (Downward leftValues, Downward rightValues) ->
            sameComponentShapes leftValues rightValues

        _ ->
            False

sameComponentShapes :: [Structure a] -> [Structure b] -> Bool
sameComponentShapes left right =
    length left == length right
        && and (zipWith sameShape left right)

-- | Mapping over a structure maps only its scalar leaves while preserving the
-- complete nesting and orientation.  This lets ordinary Haskell 'fmap' carry
-- symbolic operations through SICM structures.
instance Functor Structure where
    fmap transform structure =
        case structure of
            Scalar value ->
                Scalar (transform value)

            Upward values ->
                Upward (map (fmap transform) values)

            Downward values ->
                Downward (map (fmap transform) values)


-- | Combine two structures componentwise when they have exactly the same
-- recursive shape and orientation.
--
-- This is the structure-level analogue of 'zipWith'.  It is intentionally
-- shape-safe: mismatched structures return Nothing rather than silently
-- dropping components.
zipWithStructure
    :: (a -> b -> c)
    -> Structure a
    -> Structure b
    -> Maybe (Structure c)
zipWithStructure combine left right =
    case (left, right) of
        (Scalar leftValue, Scalar rightValue) ->
            Just (Scalar (combine leftValue rightValue))

        (Upward leftValues, Upward rightValues) ->
            Upward <$> zipChildren leftValues rightValues

        (Downward leftValues, Downward rightValues) ->
            Downward <$> zipChildren leftValues rightValues

        _ ->
            Nothing
  where
    zipChildren leftValues rightValues
        | length leftValues /= length rightValues =
            Nothing

        | otherwise =
            sequence
                (zipWith
                    (zipWithStructure combine)
                    leftValues
                    rightValues
                )
