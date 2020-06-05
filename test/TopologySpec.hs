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
    describe "addEdge" $ do
        it "Adds two vertices" $
            property (prop_addsTwoVertices T.addEdge)
        it "Creates one new Edge" $
            property (prop_addsOneEdge T.addEdge)
        it "Creates an Edge with two adjacent Vertices" $
            property (prop_EdgeHasTwoAdjacentVertices T.addEdge)
        it "Does not modify the Faces" $
            property (prop_doesNotModifyFaces T.addEdge)
    describe "addEdgeToVertex" $ do
        it "Adds a single Vertex" $
            property (prop_addsOneVertex' T.addFreeVertex T.addEdgeToVertex)
        it "Adds a single Edge" $
            property (prop_addsOneEdge' T.addFreeVertex T.addEdgeToVertex)
-- ===========================================================================
--                            Helper Functions
-- ===========================================================================
type TopoGetter a = T.Topology -> [a]
type TopoMod a b = a -> T.TopoState b
deltaXIsN :: TopoGetter b -> Int -> T.TopoState a -> T.Topology -> Bool
deltaXIsN getter n state initial = evalState test initial
    where test = do
            xs <- gets getter
            xs' <- state >> gets getter
            pure (length xs' - length xs == n)

prep_deltaXIsN :: TopoGetter a -> Int -> T.TopoState b -> TopoMod c d -> T.Topology -> Bool
prep_deltaXIsN = undefined
-- ===========================================================================
--                            Properties
-- ===========================================================================
prop_addsOneVertex :: T.TopoState a -> T.Topology -> Bool
prop_addsOneVertex = deltaXIsN T.getVertices 1

prop_addsOneVertex' :: T.TopoState a -> (a -> T.TopoState b) -> T.Topology -> Bool
prop_addsOneVertex' prep run t = evalState test t
    where test = do
            a <- prep
            vs <- gets (length . T.getVertices)
            run a
            vs' <- gets (length . T.getVertices)
            pure (vs' - vs == 1)

prop_addsTwoVertices :: T.TopoState a -> T.Topology -> Bool
prop_addsTwoVertices = deltaXIsN T.getVertices 2

prop_addsOneEdge :: T.TopoState a -> T.Topology -> Bool
prop_addsOneEdge = deltaXIsN T.getEdges 1

prop_addsOneEdge' :: T.TopoState a -> (a -> T.TopoState b) -> T.Topology -> Bool
prop_addsOneEdge' prep run t = evalState test t
    where test = do
            a <- prep
            es <- gets (length . T.getEdges)
            run a
            es' <- gets (length . T.getEdges)
            pure (es' - es == 1)

_prop_doesNotModifyVertices :: T.TopoState a -> T.Topology -> Bool
_prop_doesNotModifyVertices = deltaXIsN T.getVertices 0

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

prop_EdgeHasTwoAdjacentVertices :: T.TopoState T.Edge -> T.Topology -> Bool
prop_EdgeHasTwoAdjacentVertices s t = evalState test t
    where test = do
            e <- s
            vs <- gets (flip T.adjVertToEdge e)
            pure ((length vs) == 2)

