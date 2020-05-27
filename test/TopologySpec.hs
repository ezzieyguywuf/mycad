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
            property (prop_appendsOneToVertices addEdgeToLoop')

-- ===========================================================================
--                            Helper Functions
-- ===========================================================================
addEdgeToLoop' :: T.Topology -> T.Topology
addEdgeToLoop' t =
    let t' = T.addOpenEdgeLoop t
        (k, _) = Map.findMax $ T.getEdgeLoops t
    in T.addEdgeToLoop k t'

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes n f val = foldl (\s e -> e s ) val [f | x <- [1..n]]

addXAppendsNToY ::
    (Ord a, Eq b) =>
        (T.Topology -> T.Topology) -- what is used to append?
        -> Int -- how many are appended?
        -> (T.Topology -> Map.Map a b) -- what are they appended to?
        -> T.Topology -> Bool -- This is what is left for QuickCheck
addXAppendsNToY addX n getY t =
    let t'    = addX t
        xs    = getY t
        xs'   = getY t'
     in xs == applyNTimes n Map.deleteMax xs'

addXDoesNotModifyY ::
    (Ord a, Eq b) =>
        (T.Topology -> T.Topology)
        -> (T.Topology -> Map.Map a b)
        -> T.Topology -> Bool -- this is what is left for QuickCheck
addXDoesNotModifyY addX getY t =
    let t' = addX t
     in (getY t) ==  (getY t')

addXCreatesXWithYAdjacentZ ::
    (Ord a, Eq b, Ord c, Eq d) =>
        (T.Topology -> T.Topology)
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
prop_appendsOneToVertices :: (T.Topology -> T.Topology) -> (T.Topology -> Bool)
prop_appendsOneToVertices addX = addXAppendsNToY addX 1 T.getVertices

prop_appendsTwoToVertices :: (T.Topology -> T.Topology) -> (T.Topology -> Bool)
prop_appendsTwoToVertices addX = addXAppendsNToY addX 2 T.getVertices

prop_appendsOneToEdges :: (T.Topology -> T.Topology) -> (T.Topology -> Bool)
prop_appendsOneToEdges addX = addXAppendsNToY addX 1 T.getEdges

prop_appendsOneToFaces :: (T.Topology -> T.Topology) -> (T.Topology -> Bool)
prop_appendsOneToFaces addX = addXAppendsNToY addX 1 T.getFaces

prop_doesNotModifyEdges :: (T.Topology -> T.Topology) -> (T.Topology -> Bool)
prop_doesNotModifyEdges f = addXDoesNotModifyY f T.getEdges

prop_doesNotModifyFaces :: (T.Topology -> T.Topology) -> (T.Topology -> Bool)
prop_doesNotModifyFaces f = addXDoesNotModifyY f T.getFaces

prop_appendsOneToEdgeLoops :: (T.Topology -> T.Topology) -> (T.Topology -> Bool)
prop_appendsOneToEdgeLoops addX = addXAppendsNToY addX 1 T.getEdgeLoops

prop_hasOneAdjacentVertex :: 
    (T.Topology -> T.Topology) -> (T.Topology -> Bool)
prop_hasOneAdjacentVertex f = 
    addXCreatesXWithYAdjacentZ f T.getEdges 1 T.edgeVertices

prop_hasTwoAdjacentVertices :: 
    (T.Topology -> T.Topology) -> (T.Topology -> Bool)
prop_hasTwoAdjacentVertices f = 
    addXCreatesXWithYAdjacentZ f T.getEdges 2 T.edgeVertices

prop_hasOneAdjacentFace :: 
    (T.Topology -> T.Topology) -> (T.Topology -> Bool)
prop_hasOneAdjacentFace f = 
    addXCreatesXWithYAdjacentZ f T.getEdges 1 T.edgeFaces
