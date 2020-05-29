module TopologySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Topology as T

spec :: Spec
spec = do
    describe "addVertex" $ do
        it "Appends one to the existing Vertices" $
            property (prop_appendsOneToVertices T.addVertex)
        it "Does not modify the Edges" $
            property (prop_doesNotModifyEdges T.addVertex)
        it "Does not modify the Faces" $
            property (prop_doesNotModifyFaces T.addVertex)
    describe "makeEdge" $ do
        it "Does not modify the Vertices" $
            property (prop_doesNotModifyVertices' prepMakeEdge' makeEdge')
        it "Appends one to the existing Edges" $
            property (prop_appendsOneToEdges' prepMakeEdge' makeEdge')
        it "Does not modify the Faces" $
            property (prop_doesNotModifyFaces' prepMakeEdge' makeEdge')
        it "Creates an Edge with two adjacent Vertex" $
            property (prop_edgeHasTwoAdjacentVertex)
        it "Adjacent Vertices have one additional adjacent Edge" $
            property (prop_vertexHasOneMoreAdjacentEdge)

-- ===========================================================================
--                            Helper Functions
-- ===========================================================================
type ModTopo = (T.Topology -> T.Topology)
-- This is what QuickCheck expects
type TopoProp = (T.Topology -> Bool)

makeEdge' :: T.Topology -> T.Topology
makeEdge' t = T.makeEdge v1 v2 t
    where n  = length vs
          vs = T.getVertices t
          -- This is safe because we prep the topology with addTwoVertices.
          -- You'll get a runtime error if you forget to do this
          -- (this is a good thing, thank you haskell.)
          [v1, v2] = drop (n - 2) vs

prepMakeEdge :: T.Topology -> (T.Edge, T.Topology)
prepMakeEdge t0 = (e, t)
    where t | length (T.getVertices t0) == 0 = (T.addVertex . T.addVertex) t0
            | length (T.getVertices t0) == 1 = T.addVertex t0 
            | otherwise = t0
          t' = makeEdge' t
          e  = last $ T.getEdges t'

prepMakeEdge' :: T.Topology -> T.Topology
prepMakeEdge' t0 = t
    where (_, t) = prepMakeEdge t0

addXAppendsNToY :: Eq a => ModTopo -> Int -> (T.Topology -> [a]) -> TopoProp
addXAppendsNToY = prepXaddXAppendsNToY id

prepXaddXAppendsNToY 
    :: Eq a => ModTopo -> ModTopo -> Int -> (T.Topology -> [a]) -> TopoProp
prepXaddXAppendsNToY prepX addX n getY t0 =
    xs == take (length xs' - n) xs'
    where t   = prepX t0
          t'  = addX t
          xs  = getY t
          xs' = getY t'

addXDoesNotModifyY :: Eq a => ModTopo -> (T.Topology -> [a]) -> TopoProp
addXDoesNotModifyY = prepXaddXDoesNotModifyY id

prepXaddXDoesNotModifyY 
    :: Eq a => ModTopo -> ModTopo -> (T.Topology -> [a]) -> TopoProp
prepXaddXDoesNotModifyY prepX addX getY t0 = getY t == getY t'
    where t  = prepX t0
          t' = addX t

-- ===========================================================================
--                            Properties
-- ===========================================================================
prop_appendsOneToVertices :: ModTopo -> TopoProp
prop_appendsOneToVertices f = addXAppendsNToY f 1 T.getVertices

prop_doesNotModifyEdges :: ModTopo -> TopoProp
prop_doesNotModifyEdges f = addXDoesNotModifyY f T.getEdges

prop_doesNotModifyFaces :: ModTopo -> TopoProp
prop_doesNotModifyFaces f = addXDoesNotModifyY f T.getFaces

prop_appendsOneToEdges' :: ModTopo -> ModTopo -> TopoProp
prop_appendsOneToEdges' p f = prepXaddXAppendsNToY p f 1 T.getEdges

prop_doesNotModifyVertices' :: ModTopo -> ModTopo -> TopoProp
prop_doesNotModifyVertices' p f = prepXaddXDoesNotModifyY p f T.getVertices

prop_doesNotModifyFaces' :: ModTopo -> ModTopo -> TopoProp
prop_doesNotModifyFaces' p f = prepXaddXDoesNotModifyY p f T.getFaces

prop_edgeHasTwoAdjacentVertex :: TopoProp
prop_edgeHasTwoAdjacentVertex t0 = length (T.adjVertToEdge e t') == 2
    where (e, t) = prepMakeEdge t0
          t' = makeEdge' t

prop_vertexHasOneMoreAdjacentEdge :: TopoProp
prop_vertexHasOneMoreAdjacentEdge t0 = and $ map (== 1) [dv1, dv2]
    where (e, t) = prepMakeEdge t0
          t' = makeEdge' t
          [v1, v2] = T.adjVertToEdge e t'
          f a b = length (T.adjEdgeToVert a b)
          dv1  = (f v1 t') - (f v1 t)
          dv2  = (f v2 t') - (f v2 t)
