module GeometrySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Geometry as G
import Linear.V3

spec :: Spec
spec = do
    describe "makeLine" $ do
        it "creates a parametrized Line with u=0 as P1 and u=1 as P2" $ do
            property prop_lineParametricStartEnd
        it "a Line has constant slope" $ do
            property prop_lineConstantSlope
        it "a Line is directional in u" $ do
            property prop_lineDirectional
    describe "pointIntersectsLine" $ do
        it "Accurately determines whether or not a point lies on a line" $ do
            property prop_pointIntersectsLine
        --describe "lineIntersectsLine" $ do
            --it "checks if a Line intersects another Line, returns the intersection Point if they do" $ do
                --property prop_intersectLineLine

-- ===========================================================================
--                            Properties
-- ===========================================================================
-- Line properties
prop_lineParametricStartEnd :: Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Bool
prop_lineParametricStartEnd x1 y1 z1 x2 y2 z2 =
    let p = V3 x1 y1 z1
        q = V3 x2 y2 z2
        l = G.makeLine p q
    in (G.pointAtU l 0 == p) &&
       (G.pointAtU l 1 == q)

prop_lineConstantSlope :: G.Line Rational -> Bool
prop_lineConstantSlope l =
    let m1 = (G.pointAtU l 1) - (G.pointAtU l 0) 
        m2 = (G.pointAtU l 2) - (G.pointAtU l 1) 
        m3 = (G.pointAtU l 3) - (G.pointAtU l 4) 
        m  = G.slope l
        crosses = map (`cross` m) [m1, m2, m3]
    -- The cross product of parallel vectors is 0 (or one of them is the zero vector...)
    in all (\n -> n == 0) crosses == True

prop_lineDirectional :: G.Line Rational -> Bool
prop_lineDirectional l =
    let m1 = (G.pointAtU l 0) - (G.pointAtU l 1)
        m2 = (G.pointAtU l 1) - (G.pointAtU l 0)
    in m1 == -m2

-- Properties for intersection
prop_pointIntersectsLine :: G.Line Rational -> [Rational] -> Property
prop_pointIntersectsLine l us =
    (length us > 0) ==>
    let ps  = map (G.pointAtU l) us
        -- if we purpusefully move each point...
        ps' = map (+ (V3 1 1 1)) ps
        c1  = and $ map (G.pointIntersectsLine l) ps
        -- it should NOT intersect
        c2  = (not . and) $ map (G.pointIntersectsLine l) ps'
    in and [c1, c2]

--prop_intersectLineLine :: G.Line Rational -> Bool
--prop_intersectLineLine l =
    --let delta = V3 10 10 10
        --u0    = G.pointAtU l 0
        --u1    = G.pointAtU l 1
        --u0'   = u0 + delta
        --u1'   = u1 + delta
    ---- This line will share the starting point with l, but have a different slope
        --lint  = G.makeLine u0 u1'
    ---- This line will share the slope with l, but be offset, i.e. parallel
        --lpar  = G.makeLine u0' u1'
        --int1  = l `G.lineIntersectsLine` lint
        --int2  = l `G.lineIntersectsLine` lpar
    --in (int1 == Just u0) && (int2 == Nothing)
