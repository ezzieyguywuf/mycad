module Topology
( -- | Exported types
  Topology
, Vertex
, Face
, Edge
  -- | Exported functions
, emptyTopology
, addVertex
, makeEdge
, getVertices
, getEdges
, getFaces
)where

import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Test.QuickCheck (Arbitrary, arbitrary, elements)

-- ===========================================================================
--                               Data Types
-- ===========================================================================
newtype Topology =
    Topology {unTopology :: (Gr NodeLabel BridgeLabel)}
        deriving (Show)

newtype Vertex = Vertex Int deriving (Show, Eq)
newtype Edge = Edge Int deriving (Show, Eq)
newtype Face = Face Int deriving (Show, Eq)

type NodeLabel = (Element, Int)
newtype BridgeLabel = BridgeLabel () deriving (Show)
data Element = EVertex | EEdge | EFace deriving (Show, Eq)

-- ===========================================================================
--                               Free Functions
-- ===========================================================================
emptyTopology :: Topology
emptyTopology = Topology Graph.empty

addVertex :: Topology -> Topology
addVertex g = addNode EVertex g

makeEdge :: Vertex -> Vertex -> Topology -> Topology
makeEdge (Vertex v1) (Vertex v2) t =
    let t' = addNode EEdge t
        (Edge e)  = last $ getEdges t'
    in foldr connectNodes t' [(v1, e), (e, v1)]

getVertices :: Topology -> [Vertex]
getVertices t = map Vertex $ getData isVertex t

getEdges :: Topology -> [Edge]
getEdges t = map Edge $ getData isEdge t

getFaces :: Topology -> [Face]
getFaces t = map Face $ getData isFace t
-- ===========================================================================
--                        Private Free Functions
-- ===========================================================================
addNode :: Element -> Topology -> Topology
addNode e t = Topology $ Graph.insNode (n, e') t'
    where t' = unTopology t
          n  = length $ Graph.nodes t'
          e' = (e, n')
          n' = countNode isVertex t

connectNodes :: (Int, Int) -> Topology -> Topology
connectNodes (a, b) t =
    Topology $ Graph.insEdge (a, b, BridgeLabel ()) $ unTopology t

countNode :: (NodeLabel -> Bool) -> Topology -> Int
countNode p t = length . Graph.nodes $ Graph.labfilter p $ unTopology t

isVertex :: NodeLabel -> Bool
isVertex (EVertex, _) = True
isVertex _ = False

isEdge :: NodeLabel -> Bool
isEdge (EEdge, _) = True
isEdge _ = False

isFace :: NodeLabel -> Bool
isFace (EFace, _) = True
isFace _ = False

getData :: (NodeLabel -> Bool) -> Topology -> [Int]
getData p t =
    let t' = Graph.labfilter p $ unTopology t
    in Graph.nodes t'

-- ===========================================================================
--                            Instances
-- ===========================================================================
instance Arbitrary Topology where
    arbitrary = do
        let t0  = emptyTopology
        elements [t0]
