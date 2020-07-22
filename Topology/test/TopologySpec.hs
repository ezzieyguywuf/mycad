module TopologySpec (spec) where

import Topology
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Arbitrary, arbitrary, property)
import Control.Monad.State (execState)

spec :: Spec
spec = do
    describe "addFreeVertex" $ do
        it "Is inversed by removeVertex, resulting in original state" $
            property (prop_stateIdentity (addFreeVertex >>= removeVertex))
        --it "Adds a single Vertex to the Topology" $
            --property (prop_addsOneVertex addFreeVertex)

-- ===========================================================================
--                            Properties
-- ===========================================================================
prop_stateIdentity :: TopoState a -> TestTopology -> Bool
prop_stateIdentity run testTopology = topology == execState run topology
    where topology = unTestTopology testTopology

--prop_addsOneVertex :: TopoState a -> TestTopology -> Bool
--prop_addsOneVertex = deltaOfXisY vertices 1

--deltaOfXIsY :: TopoState a -> TestTopology -> TopoGetter a -> Int -> Bool

-- ===========================================================================
--                            Helper Functions
-- ===========================================================================
-- | In order to fix an orphaned instance warning, we'll wrap this in newtype
--   before defining a new instance
newtype TestTopology = TestTopology {unTestTopology :: Topology} deriving (Show)

--type TopoGetter a = TopoState [a]

-- | This will generate a random Topology
instance Arbitrary TestTopology where
    arbitrary = do
        nVertices <- arbitrary
        let topoState = sequence . replicate nVertices $ addFreeVertex
        pure $ TestTopology (execState topoState emptyTopology)
