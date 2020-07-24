module EntitySpec (spec) where

import Entity
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Arbitrary, arbitrary, listOf, property)
import qualified Geometry as Geo
import Linear.V3 (V3(V3))
import Control.Monad.State (runState, execState)

nullE :: Entity Float
nullE = nullEntity

spec :: Spec
spec = do
    describe "addVertex" $ do
        it "Creates a Vertex at the given Geometry" $ do
            property prop_addVertexGetPoint
        --it "allows Vertex to be retrieved using Geometry" $
            --vertex `shouldSatisfy` (`elem` getVertex entity point)
    describe "addEdge" $ do
        let (Just edge, entity) = runState (prep >>= run) nullE
            p1 = V3 10 10 10
            p2 = V3 20 20 20
            prep = do v1 <- addVertex p1
                      v2 <- addVertex p2
                      pure (v1, v2)
            run = uncurry addEdge
        it "Creates a line from v1 to v2" $ do
            let line = Geo.makeLine p1 p2
            getCurve entity edge `shouldBe` Just line
    --describe "oppositeVertex" $ do
        --let (edge, entity) = runState (addEdge p1 p2) nullE
            --p1 = V3 10 20 30
            --p2 = V3 5 10 5
            --v1 = 
        --it "Returns the Vertex on the other side of the Edge" $
            --Just (oppositeVertex e1 v1 edge) `shouldBe` Just v2
            --

-- ===========================================================================
--                            Test Properties
-- ===========================================================================
prop_addVertexGetPoint :: TestPoint Float -> TestEntity Float -> Bool
prop_addVertexGetPoint (TestPoint p) (TestEntity e) = (getPoint e' v) == (Just p)
    where (v, e') = runState (addVertex p) e

-- ===========================================================================
--                            Helper Functions
-- ===========================================================================
-- | In order to fix an orphaned instance warning, we'll wrap this in newtype
--   before defining a new instance
newtype TestEntity a = TestEntity {unTestEntity :: Entity a} deriving (Show)
newtype TestPoint  a = TestPoint {unTestPoint :: Geo.Point a} deriving (Show)

-- | This will generate a random Entity
instance (Fractional a, Arbitrary a) => Arbitrary (TestEntity a) where
    arbitrary = do
        points <- listOf arbitrary
        let entityState = sequence_ (fmap (addVertex . unTestPoint) points)
        pure $ TestEntity (execState entityState nullEntity)

-- | This will generate a random Point
instance Arbitrary a => Arbitrary (TestPoint a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        pure . TestPoint $ (V3 x y z)
