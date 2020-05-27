module TopologySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Topology as T
import qualified Data.Map as Map

spec :: Spec
spec = do
    describe "addFreeVertex" $ do
        it "Appends one to the existing Vertices" $ do
            property (prop_appendsOneToVertices T.addFreeVertex)
        it "Does not modify the Edges" $ do
            property (prop_doesNotModifyEdges T.addFreeVertex)
        it "Does not modify the Faces" $ do
            property (prop_doesNotModifyFaces T.addFreeVertex)
    describe "addRayEdge" $ do
        it "Appends one to the existing Vertices" $ do
            property (prop_appendsOneToVertices T.addRayEdge)
        it "Appends one to the existing Edges" $ do
            property (prop_appendsOneToEdges T.addRayEdge)
        it "Does not modify the Faces" $ do
            property (prop_doesNotModifyFaces T.addRayEdge)
        it "Creates an Edge with one adjacent Vertex" $ do
            property (prop_hasOneAdjacentVertex T.addRayEdge)
    describe "addLoopEdge" $ do
        it "Appends one to the existing Vertices" $ do
            property (prop_appendsOneToVertices T.addLoopEdge)
        it "Appends one to the existing Edges" $ do
            property (prop_appendsOneToEdges T.addLoopEdge)
        it "Appends one to the existing Faces" $ do
            property (prop_appendsOneToFaces T.addLoopEdge)
        it "Creates an Edge with one adjacent Vertex" $ do
            property (prop_hasOneAdjacentVertex T.addLoopEdge)
        it "Creates an Edge with one adjacent Face" $ do
            property (prop_hasOneAdjacentFace T.addLoopEdge)
    describe "addChordEdge" $ do
        it "Appends two to the existing Vertices" $ do
            property (prop_appendsTwoToVertices T.addChordEdge)
        it "Appends one to the existing Edges" $ do
            property (prop_appendsOneToEdges T.addChordEdge)
        it "Does not modify Faces" $ do
            property (prop_doesNotModifyFaces T.addChordEdge)
        it "Creates an Edge with two adjacent Vertex" $ do
            property (prop_hasTwoAdjacentVertices T.addChordEdge)
    describe "addOpenEdgeLoop" $ do
        it "Appends two to the existing Vertices" $ do
            property (prop_appendsTwoToVertices T.addOpenEdgeLoop)
        it "Appends one to the existing Edges" $ do
            property (prop_appendsOneToEdges T.addOpenEdgeLoop)
        it "Does not modify Faces" $ do
            property $ prop_doesNotModifyFaces T.addOpenEdgeLoop
        it "Appends to to existing EdgeLoops" $ do
            property (prop_appendsOneToEdgeLoops T.addOpenEdgeLoop)
    describe "addEdgeToLoop" $ do
        it "appends one to the existing Vertices" $ do
            property (prop_appendsOneToVertices' T.addOpenEdgeLoop addEdgeToLoop')
        it "appends one to the existing Edges" $ do
            property (prop_appendsOneToEdges' T.addOpenEdgeLoop addEdgeToLoop')
        it "Does not modify Faces" $ do
            property (prop_doesNotModifyFaces' T.addOpenEdgeLoop addEdgeToLoop')

-- ===========================================================================
--                            Helper Functions
-- ===========================================================================
type ModTopo = (T.Topology -> T.Topology)

addEdgeToLoop' :: T.Topology -> T.Topology
addEdgeToLoop' t =
    let (k, _) = Map.findMax $ T.getEdgeLoops t
    in T.addEdgeToLoop k t

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes n f val = foldl (\s e -> e s ) val [f | x <- [1..n]]

addXAppendsNToY ::
    (Ord a, Eq b) =>
        ModTopo -- what is used to append?
        -> Int -- how many are appended?
        -> (T.Topology -> Map.Map a b) -- what are they appended to?
        -> T.Topology -> Bool -- This is what is left for QuickCheck
addXAppendsNToY = prepXaddXAppendsNToY id

prepXaddXAppendsNToY ::
    (Ord a, Eq b) =>
        ModTopo -- what is used to prepare?
        -> ModTopo -- what is used to append?
        -> Int -- how many are appended?
        -> (T.Topology -> Map.Map a b) -- what are they appended to?
        -> T.Topology -> Bool -- This is what is left for QuickCheck
prepXaddXAppendsNToY prepX addX n getY t0 =
    let t     = prepX t0
        t'    = addX t
        xs    = getY t
        xs'   = getY t'
     in xs == applyNTimes n Map.deleteMax xs'

addXDoesNotModifyY ::
    (Ord a, Eq b) =>
        ModTopo
        -> (T.Topology -> Map.Map a b)
        -> T.Topology -> Bool -- this is what is left for QuickCheck
addXDoesNotModifyY = prepXaddXDoesNotModifyY id

prepXaddXDoesNotModifyY ::
    (Ord a, Eq b) =>
        ModTopo
        -> ModTopo
        -> (T.Topology -> Map.Map a b)
        -> T.Topology -> Bool -- this is what is left for QuickCheck
prepXaddXDoesNotModifyY prepX addX getY t0 =
    let t  = prepX t
        t' = addX t
    in (getY t) ==  (getY t')

addXCreatesXWithYAdjacentZ ::
    (Ord a, Eq b, Ord c, Eq d) =>
        ModTopo
        -> (T.Topology -> Map.Map a b)
        -> Int
        -> (a -> T.Topology -> Maybe (Map.Map c d))
        -> T.Topology -> Bool -- This part is what is left for QuickCheck
addXCreatesXWithYAdjacentZ addX getX n getAdjacentZ t =
    let t'      = addX t
        (id, _) = Map.findMax $ getX t' -- max should be the X we just added. This is ensured by prop_addXAppendsToY
        adj     = getAdjacentZ id t'
    in (length <$> adj) == Just n
-- ===========================================================================
--                            Properties
-- ===========================================================================
prop_appendsOneToVertices :: ModTopo -> (T.Topology -> Bool)
prop_appendsOneToVertices f = addXAppendsNToY f 1 T.getVertices

prop_appendsOneToVertices' :: ModTopo -> ModTopo -> (T.Topology -> Bool)
prop_appendsOneToVertices' p f = prepXaddXAppendsNToY p f 1 T.getVertices

prop_appendsTwoToVertices :: ModTopo -> (T.Topology -> Bool)
prop_appendsTwoToVertices f = addXAppendsNToY f 2 T.getVertices

prop_appendsOneToEdges :: ModTopo -> (T.Topology -> Bool)
prop_appendsOneToEdges f = addXAppendsNToY f 1 T.getEdges

prop_appendsOneToEdges' :: ModTopo -> ModTopo -> (T.Topology -> Bool)
prop_appendsOneToEdges' p f = prepXaddXAppendsNToY p f 1 T.getEdges

prop_appendsOneToFaces :: ModTopo -> (T.Topology -> Bool)
prop_appendsOneToFaces f = addXAppendsNToY f 1 T.getFaces

prop_doesNotModifyEdges :: ModTopo -> (T.Topology -> Bool)
prop_doesNotModifyEdges f = addXDoesNotModifyY f T.getEdges

prop_doesNotModifyFaces :: ModTopo -> (T.Topology -> Bool)
prop_doesNotModifyFaces f = addXDoesNotModifyY f T.getFaces

prop_doesNotModifyFaces' :: ModTopo -> ModTopo -> (T.Topology -> Bool)
prop_doesNotModifyFaces' p f = prepXaddXDoesNotModifyY p f T.getFaces

prop_appendsOneToEdgeLoops :: ModTopo -> (T.Topology -> Bool)
prop_appendsOneToEdgeLoops f = addXAppendsNToY f 1 T.getEdgeLoops

prop_hasOneAdjacentVertex :: ModTopo -> (T.Topology -> Bool)
prop_hasOneAdjacentVertex f =
    addXCreatesXWithYAdjacentZ f T.getEdges 1 T.edgeVertices

prop_hasTwoAdjacentVertices :: ModTopo -> (T.Topology -> Bool)
prop_hasTwoAdjacentVertices f =
    addXCreatesXWithYAdjacentZ f T.getEdges 2 T.edgeVertices

prop_hasOneAdjacentFace :: ModTopo -> (T.Topology -> Bool)
prop_hasOneAdjacentFace f =
    addXCreatesXWithYAdjacentZ f T.getEdges 1 T.edgeFaces
