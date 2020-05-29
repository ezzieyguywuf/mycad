module TopologySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Topology as T

spec :: Spec
spec = do
    describe "addVertex" $ do
        it "Appends one to the existing Vertices" $ do
            property (prop_appendsOneToVertices T.addVertex)
        it "Does not modify the Edges" $ do
            property (prop_doesNotModifyEdges T.addVertex)
        it "Does not modify the Faces" $ do
            property (prop_doesNotModifyFaces T.addVertex)

-- ===========================================================================
--                            Helper Functions
-- ===========================================================================
type ModTopo = (T.Topology -> T.Topology)

addXAppendsNToY ::
    ModTopo -- what is used to append?
    -> Int -- how many are appended?
    -> (T.Topology -> [T.Element]) -- what are they appended to?
    -> T.Topology -> Bool -- This is what is left for QuickCheck
addXAppendsNToY = prepXaddXAppendsNToY id

prepXaddXAppendsNToY ::
    ModTopo -- what is used to prepare?
    -> ModTopo -- what is used to append?
    -> Int -- how many are appended?
    -> (T.Topology -> [T.Element]) -- what are they appended to?
    -> T.Topology -> Bool -- This is what is left for QuickCheck
prepXaddXAppendsNToY prepX addX n getY t0 =
    let t     = prepX t0
        t'    = addX t
        xs    = getY t
        xs'   = getY t'
     in xs == take (length xs' - n) xs'

addXDoesNotModifyY ::
    ModTopo
     -> (T.Topology -> [T.Element])
     -> T.Topology -> Bool -- this is what is left for QuickCheck
addXDoesNotModifyY = prepXaddXDoesNotModifyY id

prepXaddXDoesNotModifyY ::
    ModTopo
     -> ModTopo
     -> (T.Topology -> [T.Element])
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
