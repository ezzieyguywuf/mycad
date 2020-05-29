module TopologySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Topology as T
import Data.Maybe

spec :: Spec
spec = do
    describe "addVertex" $ do
        it "Appends one to the existing Vertices" $ do
            property (prop_appendsOneToVertices T.addVertex)
        it "Does not modify the Edges" $ do
            property (prop_doesNotModifyEdges T.addVertex)
        it "Does not modify the Faces" $ do
            property (prop_doesNotModifyFaces T.addVertex)
    describe "makeEdge" $ do
        it "Does not modify the Vertices" $ do
            property (prop_doesNotModifyVertices' addTwoVertices (fromJust .T.makeEdge'))
        it "Appends one to the existing Edges" $ do
            property (prop_appendsOneToEdges' addTwoVertices (fromJust .T.makeEdge'))
        it "Does not modify the Faces" $ do
            property (prop_doesNotModifyFaces' addTwoVertices (fromJust .T.makeEdge'))
        --it "Creates an Edge with two additonal adjacent Vertex" $ do
            --property (prop_edgeHasTwoMoreAdjacentVertex)

-- ===========================================================================
--                            Helper Functions
-- ===========================================================================
type ModTopo = (T.Topology -> T.Topology)
-- This is what QuickCheck expects
type TopoProp = (T.Topology -> Bool)

addTwoVertices :: T.Topology -> T.Topology
addTwoVertices t = T.addVertex $ T.addVertex t

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
prepXaddXDoesNotModifyY prepX addX getY t0 = (getY t) == (getY t')
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

--prop_edgeHasTwoMoreAdjacentVertex :: TopoProp
--prop_edgeHasTwoMoreAdjacentVertex t = length 
    --where t' | length T.getVertices t == 0 = (T.addVertex . T.addVertex) t 
             -- | length T.getVertices t == 1 = T.addVertex t 
             -- | otherwise = t
          --[v1,v2] = t'
          --t'' = 
