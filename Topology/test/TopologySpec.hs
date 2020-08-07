{-# OPTIONS_GHC -Wno-unused-imports #-}
module TopologySpec (spec) where

import Topology
import Data.Maybe (catMaybes)
import Data.Tuple (swap)
import Data.Foldable (traverse_)
import Test.Hspec (Spec, describe, it, context, xit)
import Test.QuickCheck (Arbitrary, arbitrary, property)
import Test.QuickCheck.Gen (sublistOf)
import Control.Monad ((>=>), replicateM)
import Control.Monad.State (execState, get, put, evalState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)

spec :: Spec
spec = do
    describe "addFreeVertex" $
        it "Is inversed by removeVertex" $ do
            let run = addFreeVertex >>= removeVertex
            property (prop_runIdentity run)
    describe "vertexID" $ do
        it "provides a numeric ID that can be used to re-create the Vertex" $ do
            let run = do vs    <- getVertices
                         mVIDs <- sequence (fmap vertexID vs)
                         vs'   <- sequence (fmap vertexFromID (catMaybes mVIDs))
                         pure (vs == catMaybes vs')
            property (prop_runExpect run True)
        it "returns Nothing if the numeric ID is invalid" $ do
            let run = do topo    <- get
                         vertex  <- addFreeVertex
                         put topo -- vid should be invalid now
                         vertexID vertex
            property (prop_runExpect run Nothing)
    describe "addEdge" $ do
        let prep = do v1 <- addFreeVertex
                      v2 <- addFreeVertex
                      pure (v1, v2)
            run  = uncurry addEdge
            prepRunMaybe = prop_prepRunPostMaybeExpect prep run
        it "is inversed by removeedge" $ do
            let run' = run >=> traverse_ removeEdge
            property (prop_prepRunIdentity prep run')
        context "returns Nothing if" $ do
            let prep' = prep >>= \vals -> removeVertex (snd vals) >> pure vals
            it "the first vertex doesn't exist" $
                property (prop_prepRunExpect prep' run Nothing)
            it "the second vertex doesn't exist" $
                property (prop_prepRunExpect prep' (run . swap) Nothing)
        it "creates an Out adjacency from v1 → Edge" $ do
            let post ((v1, _), edge) = ([Out edge] == ) <$> vertexEdges v1
            property (prepRunMaybe post)
        it "creates an In adjacency for Edge ← v1" $ do
            let post ((v1, _), edge) = elem (In v1) <$> edgeVertices edge
            property (prepRunMaybe post)
        it "creates an Out adjacency for Edge → v2" $ do
            let post ((_, v2), edge) = elem (Out v2) <$> edgeVertices edge
            property (prepRunMaybe post)
        it "creates an In adjacency from v2 ← Edge" $ do
            let post ((_, v2), edge) = ([In edge] ==) <$> vertexEdges v2
            property (prepRunMaybe post)
        it "creates a single Edge adjacency on v1" $ do
            let post ((v1, _), _) = (1 ==) . length <$> vertexEdges v1
            property (prepRunMaybe post)
        it "creates a single Edge adjacency on v2" $ do
            let post ((_, v2), _) = (1 ==) . length <$> vertexEdges v2
            property (prepRunMaybe post)
        it "creates a two Vertex adjacencies on edge" $ do
            let post ((_, _), edge) = (2 ==) . length <$> edgeVertices edge
            property (prepRunMaybe post)
        it "returns the same Edge if called twice with v1→v2" $ do
            let run' args = do edge  <- run args
                               edge' <- run args
                               pure (edge == edge')
            property (prop_prepRunExpect prep run' True)
    context "an Edge from v1→v2" $ do
        let prep = do v1 <- addFreeVertex
                      v2 <- addFreeVertex
                      edge <- addEdge v1 v2
                      pure (v1, v2, edge)
            run (v1, v2, _) = addEdge v2 v1
            prepRunMaybe = prop_prepRunPostMaybeExpect prep run
        describe "addEdge from v2→v1" $ do
            it "returns the same Edge as addEdge v1 v2" $ do
                let post ((_, _, edge), edge') = pure (edge == edge')
                property (prop_prepRunPostExpect prep run post)
            xit "creates an InOut adjacency for v1 ↔ Edge" $ do
                let post ((v1, _, _), edge) = ([InOut edge] == ) <$> vertexEdges v1
                property (prepRunMaybe post)
    describe "makeEdgeLoop" $ do
        it "returns Nothing if the list of Edges do not form a loop" $ do
            let run = do vs <- replicateM 6 addFreeVertex
                         let vPairs = zip vs (tail vs)
                         es <- mapM (uncurry addEdge) vPairs
                         makeEdgeLoop (catMaybes es)
            property (prop_runExpect run Nothing)

-- ===========================================================================
--                            Properties
-- ===========================================================================
-- Represents a function that modifie the topological state
type TopoMod a b= a -> TopoState b

-- The given state should not modify anything when executed
prop_runIdentity :: TopoState a -> TestTopology -> Bool
prop_runIdentity run = prop_prepRunIdentity prep run'
    where prep  = pure ()
          run' _ = prep >> run

-- The given TopoMod should produce the given output.
prop_runExpect :: Eq a => TopoState a -> a -> TestTopology -> Bool
prop_runExpect run = prop_prepRunExpect prep run'
    where prep = pure ()
          run' _ = run

-- The state will first be "prepared" using prep, the output of which is passed
-- on to the TopoMod.
--
-- The TopoMod should not modify the state
prop_prepRunIdentity :: TopoState a -> TopoMod a b -> TestTopology -> Bool
prop_prepRunIdentity prep run testTopology = initialState == finalState
    where topology = unTestTopology testTopology
          initialState = execState prep topology
          finalState = execState (prep >>= run) topology

-- The given TopoMod should produce the given output. The TopoMod is "prepped"
-- by running the prep state, and passing along the result
prop_prepRunExpect :: Eq b => TopoState a -> TopoMod a b -> b -> TestTopology -> Bool
prop_prepRunExpect prep run val testTopology = val == producedVal
    where producedVal = evalState (prep >>= run) (unTestTopology testTopology)

-- This most generic test property allows for three stages to the test:
--
--    1. "prep" is used to produce a `State a`, and produces the initial State
--       for the test
--    2. "run" uses the `a` from the prep stage to produce a `State b`. This
--        produces the final State for the test
--    3. "post" uses produces a Bool, which is the final test condition.
--
--  Note that the "post" stage is passed both the output of the "prep" stage
--  and the output of the "run" stage, allowing for maximum flexibility.
prop_prepRunPostExpect :: TopoState a        -- ^ Produce an input for the "run" stage
                       -> TopoMod a b        -- ^ This executes the actual State under test
                       -> TopoMod (a,b) Bool -- ^ Checks the output from the "run" stage
                       -> TestTopology       -- ^ This will be provided by QuickCheck
                       -> Bool
prop_prepRunPostExpect prep run post (TestTopology topology) =
    evalState runState topology
        where runState = do a <- prep
                            b <- run a
                            post (a, b)

-- | A convenience - modifies "post" so that if "run" produced Nothing, "post"
--   returns False. Otherwise, passes along "Just b"
prop_prepRunPostMaybeExpect :: TopoState a
                            -> TopoMod a (Maybe b)
                            -> TopoMod (a,b) Bool
                            -> TestTopology
                            -> Bool
prop_prepRunPostMaybeExpect prep run post= prop_prepRunPostExpect prep run post'
        where post' (a, maybeB) = case maybeB of
                                      Just b  -> post (a,b)
                                      Nothing -> pure False

-- ===========================================================================
--                            Helper Functions
-- ===========================================================================
-- | In order to fix an orphaned instance warning, we'll wrap this in newtype
--   before defining a new instance
newtype TestTopology = TestTopology {unTestTopology :: Topology} deriving (Show)

-- | This will generate a random Topology
instance Arbitrary TestTopology where
    arbitrary = do
        nVertices    <- arbitrary
        freeVertices <- arbitrary
        let topoState = do vs <- replicateM nVertices addFreeVertex
                           replicateM freeVertices addFreeVertex
                           let vPairs = zip vs (tail vs)
                           mapM_ (uncurry addEdge) vPairs
        pure $ TestTopology (execState topoState emptyTopology)
