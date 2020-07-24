module EntitySpec (spec) where

import Entity
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck ((==>), Arbitrary, arbitrary, listOf, property, Property)
import qualified Geometry as Geo
import Linear.V3 (V3(V3))
import Control.Monad.State (runState, execState, evalState, gets)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)

spec :: Spec
spec = do
    describe "addVertex" $ do
        it "Creates a Vertex at the given Geometry" $ do
            property prop_addVertexGetPoint
    describe "addEdge" $ do
        it "Creates an Edge with an underlying Line from v1 to v2" $
            property prop_addEdgeGetLine
        it "Fails if the two points are equal" $
            property prop_addEdgeFail
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
prop_addVertexGetPoint (TestPoint p) (TestEntity e) = getPoint e' v == Just p
    where (v, e') = runState (addVertex p) e

prop_addEdgeGetLine :: TestPoint Float -> TestPoint Float -> TestEntity Float -> Property
prop_addEdgeGetLine (TestPoint p1) (TestPoint p2) (TestEntity entity) =
    p1 /= p2 ==> evalState eState entity == Just line
    where eState = runMaybeT $ do
              v1 <- lift . addVertex $ p1
              v2 <- lift . addVertex $ p2
              edge <- MaybeT (addEdge v1 v2)
              MaybeT (gets (`getCurve` edge))

          line = Geo.makeLine p1 p2

prop_addEdgeFail :: TestPoint Float -> TestEntity Float -> Bool
prop_addEdgeFail (TestPoint point) (TestEntity entity) =
    evalState eState entity == Nothing
    where eState = addVertex point >>= \v -> addEdge v v

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
