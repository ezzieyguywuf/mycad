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

-- ===========================================================================
--                            Properties
-- ===========================================================================
prop_stateIdentity :: TopoState a -> TestTopology -> Bool
prop_stateIdentity run testTopology = topology == execState run topology
    where topology = unTestTopology testTopology

-- ===========================================================================
--                            Helper Functions
-- ===========================================================================
-- | In order to fix an orphaned instance warning, we'll wrap this in newtype
--   before defining a new instance
newtype TestTopology = TestTopology {unTestTopology :: Topology} deriving (Show)

-- | This will generate a random Topology
instance Arbitrary TestTopology where
    arbitrary = do
        nVertices <- arbitrary
        let topoState = sequence . replicate nVertices $ addFreeVertex
        pure $ TestTopology (execState topoState emptyTopology)
