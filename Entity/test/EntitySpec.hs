module EntitySpec (spec) where

import Data.Either (isLeft)
import Entity
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck ((==>), Arbitrary, arbitrary, listOf, property, Property)
import qualified Geometry as Geo
import Linear.V3 (V3(V3))
import Control.Monad.State (runState, execState, evalState, gets)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Except (runExceptT, throwError, liftEither)

spec :: Spec
spec = do
    describe "addVertex" $
        it "Creates a Vertex at the given Geometry" $
            property prop_addVertexGetPoint
    describe "addEdge" $ do
        it "Creates an Edge with an underlying Line from v1 to v2" $
            property prop_addEdgeGetLine
        it "Fails if the two points are equal" $
            property prop_addEdgeFail

-- ===========================================================================
--                            Test Properties
-- ===========================================================================
prop_addVertexGetPoint :: TestPoint Float -> TestEntity Float -> Bool
prop_addVertexGetPoint (TestPoint p) (TestEntity e) = getPoint e' v == Just p
    where (v, e') = runState (addVertex p) e

prop_addEdgeGetLine :: TestPoint Float -> TestPoint Float -> TestEntity Float -> Property
prop_addEdgeGetLine (TestPoint p1) (TestPoint p2) (TestEntity entity) =
    p1 /= p2 ==> evalState eState entity == Right line
    where eState = runExceptT $ do
              -- First, get add the two vertices given the points from QuickCheck
              v1 <- lift (addVertex p1)
              v2 <- lift (addVertex p2)
              -- Try to add the Edge. This should always succeed.
              edge <- lift (addEdge v1 v2) >>= liftEither
              -- Try to retrieve the Line
              lift (gets (flip getCurve edge))
                  >>= maybe (throwError "failure") pure
          line = Geo.makeLine p1 p2

prop_addEdgeFail :: TestPoint Float -> TestEntity Float -> Bool
prop_addEdgeFail (TestPoint point) (TestEntity entity) =
    isLeft (evalState eState entity)
    where eState = addVertex point >>= \v -> addEdge v v

-- ===========================================================================
--                            Helper Functions
-- ===========================================================================
-- | In order to fix an orphaned instance warning, we'll wrap this in newtype
--   before defining a new instance
newtype TestEntity a = TestEntity {unTestEntity :: Entity a} deriving (Show)
newtype TestPoint  a = TestPoint {unTestPoint :: Geo.Point a} deriving (Show)

-- | This will generate a random Entity
instance (Fractional a, Arbitrary a, Eq a, Show a) => Arbitrary (TestEntity a) where
    arbitrary = do
        points <- listOf arbitrary
        let entityState = do vs <- mapM (addVertex . unTestPoint) points
                             let vs' = zip vs (tail vs)
                             mapM (uncurry addEdge) vs'
        pure $ TestEntity (execState entityState nullEntity)

-- | This will generate a random Point
instance Arbitrary a => Arbitrary (TestPoint a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        pure . TestPoint $ V3 x y z
