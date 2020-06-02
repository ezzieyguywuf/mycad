module Geometry
( -- | Exported Types
  Point
, Vector
, Line
, Intersection(AtPoint, Coincident, NoIntersection)
  -- | Exported Functions
, makePoint
, makeVector
, zeroVector
, dot
, cross
, makeLine
, slope
, makePlane
, pointAtU
, pointAtUV
, pointIntersectsLine
--, lineIntersectsLine
-- | These come from type classes
, pprint
, getX
, getY
, getZ
, getComponents
) where

import Data.List
import Test.QuickCheck
import qualified Data.Text as T

-- ===========================================================================
--                               Data Types
-- ===========================================================================

-- | A three-dimensional vector
data Vector a = Vector a a a deriving (Show)

-- | A wrapper to distinguish a Point from a Vector
data Point a = Point (Vector a) deriving (Show, Eq)

data Line a = Line (Point a) (Point a) deriving (Show, Eq)

data Plane a = Plane (Point a) (Point a) (Point a)

data Intersection a =
    AtPoint (Point a) 
  | Coincident
  | NoIntersection

-- ===========================================================================
--                               Type Classes
-- ===========================================================================
class PrettyPrint a where
    pprint :: a -> T.Text

class Ordinal a where
    getX :: a b -> b
    getY :: a b -> b
    getZ :: a b -> b
    getComponents :: (a b) -> [b]
    getComponents n = [getX n, getY n, getZ n]

-- ===========================================================================
--                               Functions
-- ===========================================================================

makePoint ::  (Fractional a) => a -> a -> a -> Point a
makePoint x y z = Point (Vector x y z)

makeVector :: (Fractional a) => Point a -> Point a -> Vector a
makeVector s t =
    let s' = getComponents s
        t' = getComponents t
        [x, y, z] = zipWith (-) t' s'
    in Vector x y z

zeroVector :: (Num a) => Vector a
zeroVector = Vector 0 0 0

dot :: (Num a) => Vector a -> Vector a -> a
dot a b =
    let a' = getComponents a
        b' = getComponents b
    in sum (zipWith (*) a' b')

cross :: (Num a) => Vector a -> Vector a -> Vector a
cross a b =
    let [a1, a2, a3] = getComponents a
        [b1, b2, b3] = getComponents b
        x = a2 * b3 - a3 * b2
        y = a3 * b1 - a1 * b3
        z = a1 * b2 - a2 * b1
    in Vector x y z

makeLine :: (Fractional a) => Point a -> Point a -> Line a
makeLine p1 p2 = Line p1 p2

slope :: (Fractional a) => Line a -> Vector a
slope l =
    let p = pointAtU l 0
        q = pointAtU l 1
    in makeVector p q

makePlane :: (Fractional a) => Point a -> Point a -> Point a -> Plane a
makePlane p1 p2 p3 = Plane p1 p2 p3

-- | Parametrizes a component between u=0 and u=1
componentAtU :: (Fractional a) => a -> a -> a -> a
componentAtU u c1 c2 = c1 + u * (c2 - c1)

pointAtU :: (Fractional a) => Line a -> a -> Point a
pointAtU (Line p1 p2) u =
    let p1' = getComponents p1
        p2' = getComponents p2
        [x, y, z] = zipWith (componentAtU u) p1' p2'
    in makePoint x y z

pointAtUV :: (Fractional a) => Plane a -> a -> a -> Point a
pointAtUV (Plane p1 p2 p3) u v =
    let (x1, y1, z1) = (getX p1, getY p1, getZ p1)
        (x2, y2, z2) = (getX p2, getY p2, getZ p2)
        (x3, y3, z3) = (getX p3, getY p3, getZ p3)
        x = x1 + u * (x2 - x1) + v * (x3 - x1)
        y = y1 + u * (y2 - y1) + v * (y3 - y1)
        z = z1 + u * (z2 - z1) + v * (z3 - z1)
    in makePoint x y z

-- ===========================================================================
--                      Implementations (Instances)
-- ===========================================================================
instance Ordinal Vector where
    getX (Vector x _ _ ) = x
    getY (Vector _ y _ ) = y
    getZ (Vector _ _ z ) = z

instance Ordinal Point where
    getX (Point v) = getX v
    getY (Point v) = getY v
    getZ (Point v) = getZ v

instance (Eq a) => Eq (Vector a) where
    p1 == p2 = (getX p1 == getX p2) &&
               (getY p1 == getY p2) &&
               (getZ p1 == getZ p2)

instance (Num a) => Num (Point a) where
    (Point v1) + (Point v2) = Point (v1 + v2)
    (Point v1) * (Point v2) = Point (v1 * v2)
    abs (Point v)           = Point (abs v)
    signum (Point v)        = Point (signum v)
    fromInteger i           = Point (fromInteger i)
    negate (Point v)        = Point (negate v)

instance (Num a) => Num (Vector a) where
    a + b  =
        let [x, y, z] = zipWith (+) (getComponents a) (getComponents b)
        in Vector x y z
    a * b  =
        let [x, y, z] = zipWith (*) (getComponents a) (getComponents b)
        in Vector x y z
    abs a =
        let [x, y, z] = map abs (getComponents a)
        in Vector x y z
    signum a =
        let [x, y, z] = map signum (getComponents a)
        in Vector x y z
    fromInteger i =
        let n = fromInteger i
        in Vector n n n
    negate a =
        let [x, y, z] = map negate (getComponents a)
        in Vector x y z

instance (Fractional a) => Fractional (Vector a) where
    fromRational r =
        let n = fromRational r
        in Vector n n n
    a / b =
        let [x, y, z] = zipWith (/) (getComponents a) (getComponents b)
        in Vector x y z

pointIntersectsLine :: (Fractional a, Eq a) => Line a -> Point a -> Bool
pointIntersectsLine l p =
    let u0 = pointAtU l 0
        v  = makeVector u0 p
        m  = slope l
        n  = getComponents (m / v)
    -- If all components are equal, then the Point lies on the line
    in all (\x -> x == head n) (tail n)

--lineIntersectsLine :: Line a -> Line a -> Intersection a
--lineIntersectsLine l1 l2
--    | l1 == l2         = Coincident
--    | l1 `parallel` l2 = NoIntersection
--    | l1 `offset` l2   = NoIntersection
--    | otherwise        =
        --let point  = xPoint l2 (pointAtU l1 0)
            --slopes = (slope l1) == (slope l2)
        --in point && slopes


instance (Show a) => PrettyPrint (Vector a) where
    pprint v =
        let [x, y, z] = map show $ getComponents v
            coords = concat $ intersperse "," [x, y, z]
        in T.pack $ "P(" ++ coords ++ ")"

-- Following are for QuickCheck property testing

instance (Arbitrary a, Fractional a) => Arbitrary (Point a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Point (Vector x y z))

instance (Arbitrary a, Fractional a) => Arbitrary (Vector a) where
    arbitrary = do
        p1 <- arbitrary
        p2 <- arbitrary
        return (makeVector p1 p2)

instance (Arbitrary a, Fractional a) => Arbitrary (Line a) where
    arbitrary = do
        p1 <- arbitrary
        p2 <- arbitrary
        return (makeLine p1 p2)
