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
            property (prop_doesNotModifyVertices'
                      addTwoVertices
                      (fromJust .T.makeEdge'))

-- ===========================================================================
--                            Helper Functions
-- ===========================================================================
type ModTopo = (T.Topology -> T.Topology)

addTwoVertices :: T.Topology -> T.Topology
addTwoVertices t = T.addVertex $ T.addVertex t

addXAppendsNToY ::
    (Eq a) =>
        ModTopo -- what is used to append?
        -> Int -- how many are appended?
        -> (T.Topology -> [a]) -- what are they appended to?
        -> T.Topology -> Bool -- This is what is left for QuickCheck
addXAppendsNToY = prepXaddXAppendsNToY id

prepXaddXAppendsNToY ::
    (Eq a) =>
        ModTopo -- what is used to prepare?
        -> ModTopo -- what is used to append?
        -> Int -- how many are appended?
        -> (T.Topology -> [a]) -- what are they appended to?
        -> T.Topology -> Bool -- This is what is left for QuickCheck
prepXaddXAppendsNToY prepX addX n getY t0 =
    let t     = prepX t0
        t'    = addX t
        xs    = getY t
        xs'   = getY t'
     in xs == take (length xs' - n) xs'

addXDoesNotModifyY ::
    (Eq a) =>
        ModTopo
         -> (T.Topology -> [a])
         -> T.Topology -> Bool -- this is what is left for QuickCheck
addXDoesNotModifyY = prepXaddXDoesNotModifyY id

prepXaddXDoesNotModifyY ::
    (Eq a) =>
        ModTopo
         -> ModTopo
         -> (T.Topology -> [a])
         -> T.Topology -> Bool -- this is what is left for QuickCheck
prepXaddXDoesNotModifyY prepX addX getY t0 =
    let t  = prepX t0
        t' = addX t
    in (getY t) == (getY t')

-- ===========================================================================
--                            Properties
-- ===========================================================================
prop_appendsOneToVertices :: ModTopo -> (T.Topology -> Bool)
prop_appendsOneToVertices f = addXAppendsNToY f 1 T.getVertices

prop_doesNotModifyEdges :: ModTopo -> (T.Topology -> Bool)
prop_doesNotModifyEdges f = addXDoesNotModifyY f T.getEdges

prop_doesNotModifyFaces :: ModTopo -> (T.Topology -> Bool)
prop_doesNotModifyFaces f = addXDoesNotModifyY f T.getFaces

prop_doesNotModifyVertices' :: ModTopo -> ModTopo -> (T.Topology -> Bool)
prop_doesNotModifyVertices' p f = prepXaddXDoesNotModifyY p f T.getVertices
