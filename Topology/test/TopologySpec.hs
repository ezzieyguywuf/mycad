module TopologySpec (spec) where

import Topology
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Arbitrary, arbitrary, property)
import Control.Monad.State (execState, runState)

spec :: Spec
spec = do
    describe "addFreeVertex" $
        it "Is inversed by removeVertex" $ do
            let run = addFreeVertex >>= removeVertex
            property (prop_stateIdentity run)
    describe "addEdge" $
        it "Is inversed by removeEdge" $ do
            let prep = do v1 <- addFreeVertex
                          v2 <- addFreeVertex
                          pure (v1, v2)
                run a = (uncurry addEdge) a >>= removeEdge
            property (prop_prepStateIdentity prep run)

-- ===========================================================================
--                            Properties
-- ===========================================================================
prop_stateIdentity :: TopoState a -> TestTopology -> Bool
prop_stateIdentity run = prop_prepStateIdentity prep run'
    where prep  = pure ()
          run' a = const prep a >> run

prop_prepStateIdentity :: TopoState a -> (a -> TopoState b) -> TestTopology -> Bool
prop_prepStateIdentity prep run testTopology = initialState == finalState
    where (args, initialState) = runState prep (unTestTopology testTopology)
          finalState = execState (run args) initialState

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
