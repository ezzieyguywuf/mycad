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
        it "Does not modify Vertices" $
            property (prop_doesNotModifyVertices T.addFreeEdge)
        it "Creates one new Edge" $
            property (prop_addsOneEdge T.addFreeEdge)
        it "Does not modify the Faces" $
            property (prop_doesNotModifyFaces T.addFreeEdge)
    describe "boundFreeEdge" $ do
        let s = T.addFreeEdge >>= T.boundFreeEdge
        it "Creates two new Vertices" $
            property (prop_addsTwoVertex s)
        it "Does not add or delete Edges" $
            property (prop_doesNotAddOrDeleteEdges T.addFreeEdge T.boundFreeEdge)
        it "Creates a single adjacent Vertex to the Edge" $
            property (prop_vertexHasOneAdjacentEdge s)
        it "Does not modify Faces" $
            property (prop_doesNotModifyFaces s)

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

prop_addsTwoVertex :: T.TopoState a -> T.Topology -> Bool
prop_addsTwoVertex s t = (vs' - vs) == 2
    where vs = length $ T.getVertices t
          vs' = length $ T.getVertices $ execState s t

prop_doesNotModifyVertices :: T.TopoState a -> T.Topology -> Bool
prop_doesNotModifyVertices s t = vs == vs'
    where vs  = T.getVertices t
          vs' = T.getVertices $ execState s t

prop_doesNotModifyEdges :: T.TopoState a -> T.Topology -> Bool
prop_doesNotModifyEdges s t = es == es'
    where es  = T.getEdges t
          es' = T.getEdges $ execState s t

prop_doesNotModifyFaces :: T.TopoState a -> T.Topology -> Bool
prop_doesNotModifyFaces s t = es == es'
    where es  = T.getFaces t
          es' = T.getFaces $ execState s t

prop_doesNotAddOrDeleteEdges :: T.TopoState T.Edge -> (T.Edge -> T.TopoState a) -> T.Topology -> Bool
prop_doesNotAddOrDeleteEdges prep run t0 = evalState test t0
    where test = do
            t <- prep
            es <- (length . T.getEdges) <$> get
            run t
            es' <- (length . T.getEdges) <$> get
            pure (es == es')

prop_addsOneEdge :: T.TopoState a -> T.Topology -> Bool
prop_addsOneEdge s t = (es' - es) == 1
    where es = length $ T.getEdges t
          es' = length $ T.getEdges $ execState s t

prop_vertexHasOneAdjacentEdge :: T.TopoState T.Vertex -> T.Topology -> Bool
prop_vertexHasOneAdjacentEdge s t = evalState test t
    where test = do
            v <- s
            adj <- gets (flip T.adjEdgeToVert v)
            pure ((length adj) == 1)
