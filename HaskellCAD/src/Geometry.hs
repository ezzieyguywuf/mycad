module Geometry
( -- | Exported Types
  Point
, Vector
, Line
, Intersection(AtPoint, Coincident, NoIntersection)
  -- | Exported Functions
, zeroVector
, makeLine
, slope
, makePlane
, pointAtU
, pointAtUV
, pointIntersectsLine
) where

import Test.QuickCheck
import Linear.V3
import Linear.Vector (lerp)

-- ===========================================================================
--                               Data Types
-- ===========================================================================

-- | A three-dimensional vector
type Vector a = V3 a

-- | A wrapper to distinguish a Point from a Vector
type Point a = Vector a

data Line a = Line (Point a) (Point a) deriving (Show, Eq)

data Plane a = Plane (Point a) (Point a) (Point a)

data Intersection a =
    AtPoint (Point a) 
  | Coincident
  | NoIntersection

-- ===========================================================================
--                               Functions
-- ===========================================================================

zeroVector :: (Num a) => Vector a
zeroVector = V3 0 0 0

makeLine :: (Fractional a) => Point a -> Point a -> Line a
makeLine p1 p2 = Line p1 p2

slope :: (Fractional a) => Line a -> Vector a
slope l = q - p
    where p = pointAtU l 0
          q = pointAtU l 1

makePlane :: (Fractional a) => Point a -> Point a -> Point a -> Plane a
makePlane p1 p2 p3 = Plane p1 p2 p3

-- | Parametrizes a component between u=0 and u=1
pointAtU :: (Fractional a) => Line a -> a -> Point a
pointAtU (Line p1 p2) u = lerp u p2 p1

pointAtUV :: (Fractional a) => Plane a -> a -> a -> Point a
pointAtUV (Plane p1 p2 p3) u v =
    let (V3 x1 y1 z1) = p1
        (V3 x2 y2 z2) = p2
        (V3 x3 y3 z3) = p3
        x = x1 + u * (x2 - x1) + v * (x3 - x1)
        y = y1 + u * (y2 - y1) + v * (y3 - y1)
        z = z1 + u * (z2 - z1) + v * (z3 - z1)
    in V3 x y z

-- ===========================================================================
--                      Implementations (Instances)
-- ===========================================================================
pointIntersectsLine :: (Fractional a, Eq a) => Line a -> Point a -> Bool
pointIntersectsLine l p = (m `cross` v) == (V3 0 0 0)
    where u0 = pointAtU l 0
          v  = p - u0
          m  = slope l

--lineIntersectsLine :: Line a -> Line a -> Intersection a
--lineIntersectsLine l1 l2
--    | l1 == l2         = Coincident
--    | l1 `parallel` l2 = NoIntersection
--    | l1 `offset` l2   = NoIntersection
--    | otherwise        =
        --let point  = xPoint l2 (pointAtU l1 0)
            --slopes = (slope l1) == (slope l2)
        --in point && slopes


-- Following are for QuickCheck property testing

instance (Arbitrary a, Fractional a) => Arbitrary (Line a) where
    arbitrary = do
        (x1, y1, z1) <- arbitrary
        (x2, y2, z2) <- arbitrary
        return (makeLine (V3 x1 y1 z1) (V3 x2 y2 z2))
