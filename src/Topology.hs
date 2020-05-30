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
, adjVertToEdge
, adjEdgeToVert
, getVertices
, getEdges
, getFaces
, prettyPrintVertex
, prettyPrintTopology
)where

import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Test.QuickCheck (Arbitrary, arbitrary, elements)
import qualified Data.Text as Text

-- ===========================================================================
--                               Data Types
-- ===========================================================================
newtype Topology =
    Topology {unTopology :: Gr NodeLabel BridgeLabel}
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
addVertex = addNode EVertex

makeEdge :: Vertex -> Vertex -> Topology -> Topology
makeEdge (Vertex v1) (Vertex v2) t =
    let t' = addNode EEdge t
        (Edge e)  = last $ getEdges t'
    in foldr connectNodes t' [(v1, e), (e, v2)]

adjVertToEdge :: Edge -> Topology -> [Vertex]
adjVertToEdge (Edge n) t = map Vertex ns
    where t' = unTopology $ getSubGraph (not . isFace) t
          ns = Graph.neighbors t' n

adjEdgeToVert :: Vertex -> Topology -> [Edge]
adjEdgeToVert (Vertex n) t = map Edge ns
    where t' = unTopology $ getSubGraph (not . isFace) t
          ns = Graph.neighbors t' n

prettyPrintVertex :: Vertex -> Topology -> Text.Text
prettyPrintVertex (Vertex i) t = Text.pack $ "V" ++ show i

prettyPrintTopology :: Topology -> Text.Text
prettyPrintTopology t = Text.pack $ Graph.prettify $ unTopology t

getVertices :: Topology -> [Vertex]
getVertices t = map Vertex $ getNodes isVertex t

getEdges :: Topology -> [Edge]
getEdges t = map Edge $ getNodes isEdge t

getFaces :: Topology -> [Face]
getFaces t = map Face $ getNodes isFace t
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

getSubGraph :: (NodeLabel -> Bool) -> Topology -> Topology
getSubGraph p t = Topology $ Graph.labfilter p $ unTopology t

getNodes :: (NodeLabel -> Bool) -> Topology -> [Int]
getNodes p t = Graph.nodes $ unTopology $ getSubGraph p t

-- ===========================================================================
--                            Instances
-- ===========================================================================
instance Arbitrary Topology where
    arbitrary = elements [t0, t1, t2, t3, t4]
        where t0 = emptyTopology
              t1 = addVertex emptyTopology
              t2 = addVertex t1
              t3 = makeEdge (Vertex 0) (Vertex 1) t2
              t4 = addVertex $ addVertex t3
