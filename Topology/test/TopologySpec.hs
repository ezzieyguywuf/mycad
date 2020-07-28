module TopologySpec (spec) where

import Topology
import Data.Tuple (swap)
import Data.Foldable (traverse_)
import Test.Hspec (Spec, describe, it, context, xit)
import Test.QuickCheck (Arbitrary, arbitrary, property)
import Control.Monad ((>=>))
import Control.Monad.State (execState, get, put, evalState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)

spec :: Spec
spec = do
    describe "addFreeVertex" $
        it "Is inversed by removeVertex" $ do
            let run = addFreeVertex >>= removeVertex
            property (prop_stateIdentity run)
    describe "vertexID" $ do
        it "provides a numeric ID that can be used to re-create the Vertex" $ do
            let run = runMaybeT $ do vertex  <- lift addFreeVertex
                                     vid     <- MaybeT (vertexID vertex)
                                     vertex' <- MaybeT (vertexFromID vid)
                                     pure (vertex == vertex')
            property (prop_stateExpect run (Just True))
        it "returns Nothing if the numeric ID is invalid" $ do
            let run = do topo    <- get
                         vertex  <- addFreeVertex
                         put topo -- vid should be invalid now
                         vertexID vertex
            property (prop_stateExpect run Nothing)
    describe "addEdge" $ do
        let prep = do v1 <- addFreeVertex
                      v2 <- addFreeVertex
                      pure (v1, v2)
            run  = uncurry addEdge
        it "is inversed by removeedge" $ do
            let run' = run >=> traverse_ removeEdge
            property (prop_prepStateIdentity prep run')
        context "returns Nothing if" $ do
            let prep' = prep >>= \vals -> removeVertex (snd vals) >> pure vals
            it "the first vertex doesn't exist" $
                property (prop_prepStateExpect prep' run Nothing)
            it "the second vertex doesn't exist" $
                property (prop_prepStateExpect prep' (run . swap) Nothing)
        it "creates an Out adjacency from v1 → Edge" $ do
            let run' vs@(v1,_)= runMaybeT $ do
                    edge <- MaybeT (run vs)
                    lift (vertexEdges v1) >>= pure . ([Out edge] ==)
            property (prop_prepStateExpect prep run' (Just True))
        xit "returns the same Edge if called twice with v1→v2" $ do
            let run' args = do edge  <- run args
                               edge' <- run args
                               pure (edge == edge')
            property (prop_prepStateExpect prep run' True)

-- ===========================================================================
--                            Properties
-- ===========================================================================
-- Represents a function that modifie the topological state
type TopoMod a b= a -> TopoState b

-- The given state should not modify anything when executed
prop_stateIdentity :: TopoState a -> TestTopology -> Bool
prop_stateIdentity run = prop_prepStateIdentity prep run'
    where prep  = pure ()
          run' a = const prep a >> run

-- The state will first be "prepared" using prep, the output of which is passed
-- on to the TopoMod.
--
-- The TopoMod should not modify the state
prop_prepStateIdentity :: TopoState a -> TopoMod a b -> TestTopology -> Bool
prop_prepStateIdentity prep run testTopology = initialState == finalState
    where topology = unTestTopology testTopology
          initialState = execState prep topology
          finalState = execState (prep >>= run) topology

-- The given TopoMod should produce the given output.
prop_stateExpect :: Eq a => TopoState a -> a -> TestTopology -> Bool
prop_stateExpect run val = prop_prepStateExpect prep run' val
    where prep = pure ()
          run' _ = run

-- The given TopoMod should produce the given output. The TopoMod is "prepped"
-- by running the prep state, and passing along the result
prop_prepStateExpect :: Eq b => TopoState a -> TopoMod a b -> b -> TestTopology -> Bool
prop_prepStateExpect prep run val testTopology = val == producedVal
    where producedVal = evalState (prep >>= run) (unTestTopology testTopology)


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
