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
    describe "addHalfEdge" $ do
        it "Adds two vertices" $
            property (prop_addsTwoVertices T.addHalfEdge)
        it "Creates one new Edge" $
            property (prop_addsOneEdge T.addHalfEdge)
        it "Creates an Edge with two adjacent Vertices" $
            property (prop_halfEdgeHasTwoAdjacentVertices T.addHalfEdge)
        it "Does not modify the Faces" $
            property (prop_doesNotModifyFaces T.addHalfEdge)
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

_prop_doesNotModifyVertices :: T.TopoState a -> T.Topology -> Bool
_prop_doesNotModifyVertices s t = vs == vs'
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

_prop_doesNotAddOrDeleteEdges :: T.TopoState T.Edge -> (T.Edge -> T.TopoState a) -> T.Topology -> Bool
_prop_doesNotAddOrDeleteEdges prep run t0 = evalState test t0
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

prop_halfEdgeHasTwoAdjacentVertices :: T.TopoState T.Edge -> T.Topology -> Bool
prop_halfEdgeHasTwoAdjacentVertices s t = evalState test t
    where test = do
            e <- s
            vs <- gets (flip T.adjVertToEdge e)
            pure ((length vs) == 2)
