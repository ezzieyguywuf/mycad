module TopologySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Topology as T
import Control.Monad.State

spec :: Spec
spec = do
    describe "addFreeVertex" $ do
        it "Creates one new Vertex" $
            property (prop_addsOneVertex T.addFreeVertex)
        it "Does not modify the Edges" $
            property (prop_doesNotModifyEdges T.addFreeVertex)
        it "Does not modify the Faces" $
            property (prop_doesNotModifyFaces T.addFreeVertex)
    describe "addFreeEdge" $ do
        it "Creates two new Vertices" $
            property (prop_addsTwoVertices T.addFreeEdge)
        it "Creates one new Edge" $
            property (prop_addsOneEdge T.addFreeEdge)
        it "Does not modify the Faces" $
            property (prop_doesNotModifyFaces T.addFreeEdge)
        it "Creates an Edge with two adjacent Vertex" $
            property (prop_edgeHasTwoAdjacentVertex T.addFreeEdge)

-- ===========================================================================
--                            Helper Functions
-- ===========================================================================
-- ===========================================================================
--                            Properties
-- ===========================================================================
prop_addsOneVertex :: T.TopoState a -> T.Topology -> Bool
prop_addsOneVertex s t = (vs' - vs) == 1
    where vs = length $ T.getVertices t
          vs' = length $ T.getVertices $ execState s t

prop_addsTwoVertices :: T.TopoState a -> T.Topology -> Bool
prop_addsTwoVertices s t = (vs' - vs) == 2
    where vs = length $ T.getVertices t
          vs' = length $ T.getVertices $ execState s t

prop_doesNotModifyEdges :: T.TopoState a -> T.Topology -> Bool
prop_doesNotModifyEdges s t = es == es'
    where es  = T.getEdges t
          es' = T.getEdges $ execState s t

prop_doesNotModifyFaces :: T.TopoState a -> T.Topology -> Bool
prop_doesNotModifyFaces s t = es == es'
    where es  = T.getFaces t
          es' = T.getFaces $ execState s t

prop_addsOneEdge :: T.TopoState a -> T.Topology -> Bool
prop_addsOneEdge s t = (es' - es) == 1
    where es = length $ T.getEdges t
          es' = length $ T.getEdges $ execState s t

prop_edgeHasTwoAdjacentVertex :: T.TopoState T.Edge -> T.Topology -> Bool
prop_edgeHasTwoAdjacentVertex s t = length (T.adjVertToEdge t' e) == 2
    where (e, t') = runState s t
