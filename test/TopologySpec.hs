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
        let prep = prepMakeEdge' 
            make = makeEdge'
        it "Does not modify the Vertices" $
            property (prop_doesNotModifyVertices' prep make)
        it "Appends one to the existing Edges" $
            property (prop_appendsOneToEdges' prep make)
        it "Does not modify the Faces" $
            property (prop_doesNotModifyFaces' prep make)
        it "Creates an Edge with two adjacent Vertex" $
            property (prop_edgeHasTwoAdjacentVertex)
        it "Target Vertices have one additional adjacent Edge" $
            property (prop_vertexHasOneMoreAdjacentEdge)
        it "Creates an Edge with zero adjacent Face" $
            property (prop_edgeHasZeroAdjacentFace prep make)
        context "an Edge already exists" $ do
            it "Does not modify the Edges" $
                property (prop_doesNotModifyEdges' (make . prep) make)
            context "we try to make an Edge from v2 to v1" $
                it "Appends one to the existing Edges" $
                    property (prop_appendsOneToEdges' prep makeEdge'2)

-- ===========================================================================
--                            Helper Functions
-- ===========================================================================
type ModTopo = T.Topology -> T.Topology
type GetElem a = T.Topology -> [a]
-- This is what QuickCheck expects
type TopoProp = T.Topology -> Bool

makeEdge' :: T.Topology -> T.Topology
makeEdge' t = makeEdgeHelper True t

makeEdge'2 :: T.Topology -> T.Topology
makeEdge'2 t = makeEdgeHelper False t

makeEdgeHelper :: Bool -> T.Topology -> T.Topology
makeEdgeHelper b t
    | b         = T.makeEdge v1 v2 t
    | otherwise = T.makeEdge v2 v1 t
    where n  = length vs
          vs = T.getVertices t
          -- This is safe because we prep the topology with addTwoVertices.
          -- You'll get a runtime error if you forget to do this
          -- (this is a good thing, thank you haskell.)
          [v1, v2] = drop (n - 2) vs

prepMakeEdge :: T.Topology -> (T.Edge, T.Topology)
prepMakeEdge t0 = (e, t)
    where t  = T.addVertex . T.addVertex $ t0
          t' = makeEdge' t
          e  = last $ T.getEdges t'

prepMakeEdge' :: T.Topology -> T.Topology
prepMakeEdge' t0 = t where (_, t) = prepMakeEdge t0

makeTopo :: ModTopo -> ModTopo -> GetElem a -> T.Topology -> ([a], T.Topology)
makeTopo p f g t0 = (as, t')
    where t  = p t0
          t' = f t
          as =  g t

addXAppendsNToY :: Eq a => ModTopo -> Int -> GetElem a -> TopoProp
addXAppendsNToY = prepXaddXAppendsNToY id

prepXaddXAppendsNToY :: Eq a => ModTopo -> ModTopo -> Int -> GetElem a -> TopoProp
prepXaddXAppendsNToY p f n g t0 = xs == xs'
    where (xs, t') = makeTopo p f g t0
          xs_all = g t'
          xs' = take (length xs_all - n) xs_all

addXDoesNotModifyY :: Eq a => ModTopo -> GetElem a -> TopoProp
addXDoesNotModifyY = prepXaddXDoesNotModifyY id

prepXaddXDoesNotModifyY :: Eq a => ModTopo -> ModTopo -> GetElem a -> TopoProp
prepXaddXDoesNotModifyY p f g t0 = xs == xs'
    where (xs, t') = makeTopo p f g t0
          xs' = g t'

prepXMakeXHasNAdjacentY :: Eq a => ModTopo -> ModTopo -> Int -> GetElem a -> TopoProp
prepXMakeXHasNAdjacentY p f n g t0 = (length xs') == n
    where (_, t') = makeTopo p f g t0
          xs' = g t'

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

prop_doesNotModifyEdges' :: ModTopo -> ModTopo -> TopoProp
prop_doesNotModifyEdges' p f = prepXaddXDoesNotModifyY p f T.getEdges

prop_doesNotModifyFaces' :: ModTopo -> ModTopo -> TopoProp
prop_doesNotModifyFaces' p f = prepXaddXDoesNotModifyY p f T.getFaces

prop_edgeHasTwoAdjacentVertex :: TopoProp
prop_edgeHasTwoAdjacentVertex t0 = length (T.adjVertToEdge t' e) == 2
    where (e, t) = prepMakeEdge t0
          t' = makeEdge' t

prop_vertexHasOneMoreAdjacentEdge :: TopoProp
prop_vertexHasOneMoreAdjacentEdge t0 = and $ map (== 1) [dv1, dv2]
    where (e, t) = prepMakeEdge t0
          t' = makeEdge' t
          [v1, v2] = T.adjVertToEdge t' e
          f a b = length (T.adjEdgeToVert a b)
          dv1  = (f t' v1) - (f t v1)
          dv2  = (f t' v2) - (f t v2)

prop_edgeHasZeroAdjacentFace :: ModTopo -> ModTopo -> TopoProp
prop_edgeHasZeroAdjacentFace p f = prepXMakeXHasNAdjacentY p f 0 T.getFaces
