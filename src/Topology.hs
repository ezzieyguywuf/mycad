module Topology
( -- | Exported types
  Topology
, Element(..)
  -- | Exported functions
, emptyTopology
, addVertex
, getVertices
, getEdges
, getFaces
)where

import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Maybe
import Test.QuickCheck (Arbitrary, arbitrary, elements)

-- ===========================================================================
--                               Data Types
-- ===========================================================================
type NodeLabel = (Element, Int)
newtype BridgeLabel = BridgeLabel () deriving (Show)
newtype Topology =
    Topology {unTopology :: (Gr NodeLabel BridgeLabel)}
        deriving (Show)

data Element = Vertex Int
             | Edge   Int
             | Face   Int
             deriving (Show, Eq)

-- ===========================================================================
--                               Free Functions
-- ===========================================================================
emptyTopology :: Topology
emptyTopology = Topology Graph.empty

addVertex :: Topology -> Topology
addVertex g = addNode Vertex g

getVertices :: Topology -> [Element]
getVertices t = getData isVertex t

getEdges :: Topology -> [Element]
getEdges t = getData isEdge t

getFaces :: Topology -> [Element]
getFaces t = getData isFace t
-- ===========================================================================
--                        Private Free Functions
-- ===========================================================================
addNode :: (Int -> Element) -> Topology -> Topology
addNode e t = Topology $ Graph.insNode (n, e') t'
    where t' = unTopology t
          n  = length $ Graph.nodes t'
          e' = (e n, n')
          n' = countNode isVertex t

countNode :: (NodeLabel -> Bool) -> Topology -> Int
countNode p t = length . Graph.nodes $ Graph.labfilter p $ unTopology t

isVertex :: NodeLabel -> Bool
isVertex (Vertex _, _) = True
isVertex _ = False

isEdge :: NodeLabel -> Bool
isEdge (Edge _, _) = True
isEdge _ = False

isFace :: NodeLabel -> Bool
isFace (Face _, _) = True
isFace _ = False

getData :: (NodeLabel -> Bool) -> Topology -> [Element]
getData p t =
    let t' = unTopology t
        xs = Graph.nodes $ Graph.labfilter p t'
    in map fst $ mapMaybe (Graph.lab t') xs

-- ===========================================================================
--                            Instances
-- ===========================================================================
instance Arbitrary Topology where
    arbitrary = do
        let t0  = emptyTopology
        elements [t0]
