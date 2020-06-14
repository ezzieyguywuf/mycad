module GeometrySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Geometry as G

spec :: Spec
spec = do
    describe "makePoint" $ do
        it "creates a point with the expected coordinates" $ do
            property prop_pointCoordinates
    describe "makeVector" $ do
        it "creates a vector with the expected offsets" $ do
            property prop_vectorOffsets
    context "Vector addition must satisfy certain axioms" $ do
        describe "(+)" $ do
            it "is communative" $
                property prop_vectAddComm
            it "is associative" $
                property prop_vectAddAssoc
            it "has an identity with the zero vector" $
                property prop_vectAddIdent
            it "cancels out with negation" $
                property prop_vectAddInverse
    context "Vector scalar multiplication must satisfy certain axioms" $ do
        describe "(*)" $ do
            it "has an identity with scalar 1" $
                property prop_vectMultIdent
            it "is associative with scalars" $
                property prop_vectMultAssoc
            it "is distributive with scalars" $
                property prop_vectMultDistrib
            it "is distributive with scalar addition" $
                property prop_vectMultDistrib'
    context "the Vector dot product must satisfy certain axioms" $ do
        describe "dot" $ do
            it "is communative" $
                property prop_vectDotComm
            it "is distributive with vector addition" $
                property prop_vectDotDistrib
            it "is bilinear" $
                property prop_vectDotBilinear
            it "is communative with scalar multiplication" $
                property prop_vectDotScalDistrib
    context "the Vector cross product must satisfy certain axioms" $ do
        describe "cross" $ do
            it "returns zero when used with the same Vector" $
                property prop_vectCrossSelf
            it "is anticommunitive" $
                property prop_vectCrossAntiComm
            it "is distributive with Vector addition" $
                property prop_vectCrossDistrib
            it "is compatible with scalar multiplication" $
                property prop_vectCrossScalMult
            it "satisfies the Jacobi principle" $
                property prop_vectCrossJacobi
    describe "makeLine" $ do
        it "creates a parametrized Line with u=0 as P1 and u=1 as P2" $ do
            property prop_lineParametricStartEnd
        it "a Line has constant slope" $ do
            property prop_lineConstantSlope
        it "a Line is directional in u" $ do
            property prop_lineDirectional
        it "a Lines parameter will return an arbitrary multiple of the original interval" $ do
            property prop_lineParameterMultiple
    context "given two geometric entities, we should be able to determine if they intersect" $ do
        describe "pointIntersectsLine" $ do
            it "checks if a Point intersects a given Line" $ do
                property prop_intersectPointLine
        --describe "lineIntersectsLine" $ do
            --it "checks if a Line intersects another Line, returns the intersection Point if they do" $ do
                --property prop_intersectLineLine

-- ===========================================================================
--                            Properties
-- ===========================================================================

-- Vector additive properties
prop_vectAddComm :: G.Vector Rational -> G.Vector Rational -> Bool
prop_vectAddComm a b = a + b == b + a

prop_vectAddAssoc :: G.Vector Rational -> G.Vector Rational -> G.Vector Rational -> Bool
prop_vectAddAssoc a b c= a + (b + c) == (a + b) + c

prop_vectAddIdent :: G.Vector Rational -> Bool
prop_vectAddIdent a =
    let z = G.zeroVector
    in (a + z == a) && (a + z == z + a)

prop_vectAddInverse :: G.Vector Rational -> Bool
prop_vectAddInverse a = a + (-a) == G.zeroVector

-- Vector scalar multiplicative properties
prop_vectMultIdent :: G.Vector Rational -> Bool
prop_vectMultIdent a = 1 * a == a

prop_vectMultAssoc :: G.Vector Rational -> Rational -> Rational -> Bool
prop_vectMultAssoc a r s =
    let r' = fromRational r
        s' = fromRational s
    in r' * (s' * a) == (r' * s') * a

prop_vectMultDistrib :: G.Vector Rational -> G.Vector Rational -> Rational -> Bool
prop_vectMultDistrib a b r =
    let r' = fromRational r
    in r' * (a + b) == (r' * a) + (r' * b)

prop_vectMultDistrib' :: G.Vector Rational -> Rational -> Rational -> Bool
prop_vectMultDistrib' a r s =
    let r' = fromRational r
        s' = fromRational s
    in (r' + s') * a == (r' * a) + (s' * a)

-- Vector dot product properties
prop_vectDotComm :: G.Vector Rational -> G.Vector Rational -> Bool
prop_vectDotComm a b = a `G.dot` b == b `G.dot` a

prop_vectDotDistrib :: G.Vector Rational -> G.Vector Rational -> G.Vector Rational -> Bool
prop_vectDotDistrib a b c = a `G.dot` (b + c) == (a `G.dot` b) + (a `G.dot` c)

prop_vectDotBilinear :: G.Vector Rational -> G.Vector Rational -> G.Vector Rational -> Rational -> Bool
prop_vectDotBilinear a b c r =
    let r' = fromRational r
    in a `G.dot` (r' * b + c) == r * (a `G.dot` b) + (a `G.dot` c)

prop_vectDotScalDistrib :: G.Vector Rational -> G.Vector Rational -> Rational -> Rational -> Bool
prop_vectDotScalDistrib a b r s=
    let r' = fromRational r
        s' = fromRational s
    in (r' * a) `G.dot`(s' * b) == (r * s) * (a `G.dot` b)

-- Vector cross product properties
prop_vectCrossSelf :: G.Vector Rational -> Bool
prop_vectCrossSelf a = a `G.cross` a == 0

prop_vectCrossAntiComm :: G.Vector Rational -> G.Vector Rational -> Bool
prop_vectCrossAntiComm a b = a `G.cross` b == -(b `G.cross` a)

prop_vectCrossDistrib :: G.Vector Rational -> G.Vector Rational -> G.Vector Rational -> Bool
prop_vectCrossDistrib a b c = a `G.cross` (b + c) == (a `G.cross` b) + (a `G.cross` c)

prop_vectCrossScalMult :: G.Vector Rational -> G.Vector Rational -> Rational -> Bool
prop_vectCrossScalMult a b r =
    let r'  = fromRational r
        lhs = (r' * a) `G.cross` b
    in (lhs == a `G.cross` (r' * b)) &&
       (lhs == r' * (a `G.cross` b))

prop_vectCrossJacobi :: G.Vector Rational -> G.Vector Rational -> G.Vector Rational -> Bool
prop_vectCrossJacobi a b c =
    let a' = a `G.cross` (b `G.cross` c)
        b' = b `G.cross` (c `G.cross` a)
        c' = c `G.cross` (a `G.cross` b)
    in a' + b' + c' == 0

-- Point and Vector properties
prop_pointCoordinates :: Rational -> Rational -> Rational -> Bool
prop_pointCoordinates x y z = G.getComponents (G.makePoint x y z) == [x, y, z]

prop_vectorOffsets :: G.Point Rational -> G.Point Rational -> Bool
prop_vectorOffsets p q =
    let check = zipWith (-) (G.getComponents q) (G.getComponents p)
    in (G.getComponents (G.makeVector p q)) == check

-- Line properties
prop_lineParametricStartEnd :: G.Point Rational -> G.Point Rational -> Bool
prop_lineParametricStartEnd p q =
    let l = G.makeLine p q
    in (G.pointAtU l 0 == p) &&
       (G.pointAtU l 1 == q)

prop_lineConstantSlope :: G.Line Rational -> Bool
prop_lineConstantSlope l =
    let m1 = G.makeVector (G.pointAtU l 0) (G.pointAtU l 1)
        m2 = G.makeVector (G.pointAtU l 1) (G.pointAtU l 2)
        m3 = G.makeVector (G.pointAtU l 4) (G.pointAtU l 3)
        m  = G.slope l
        crosses = map (`G.cross` m) [m1, m2, m3]
    -- The cross product of parallel vectors is 0 (or one of them is the zero vector...)
    in all (\n -> n == 0) crosses == True

prop_lineDirectional :: G.Line Rational -> Bool
prop_lineDirectional l =
    let m1 = (G.pointAtU l 0) - (G.pointAtU l 1)
        m2 = (G.pointAtU l 1) - (G.pointAtU l 0)
    in m1 == -m2

prop_lineParameterMultiple :: G.Line Rational-> [Rational] -> Bool
prop_lineParameterMultiple l us =
    let p0 = G.pointAtU l 0
        m  = G.slope l
        -- Find the Point at each randomly generated u
        ps = map (G.pointAtU l) us
        -- Create a vector from p0 to each random Point
        vs = map (G.makeVector p0) ps
        -- Each vector created will be a multiple of the basis vector m
        mults = map (/ m) vs
        -- These deltas should all be Vector 0 0 0
        deltas = zipWith (-) mults (map fromRational us)
     in all (\v -> v == G.zeroVector) deltas

-- Properties for intersection
prop_intersectPointLine :: G.Line Rational -> [Rational] -> Property
prop_intersectPointLine l us =
    (length us > 0) ==>
    let ps  = map (G.pointAtU l) us
        -- if we purpusefully move each point...
        ps' = map (+ (G.makePoint 1 1 1)) ps
        c1  = and $ map (G.pointIntersectsLine l) ps
        -- it should NOT intersect
        c2  = (not . and) $ map (G.pointIntersectsLine l) ps'
    in and [c1, c2]

--prop_intersectLineLine :: G.Line Rational -> Bool
--prop_intersectLineLine l =
    --let delta = G.makePoint 10 10 10
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
