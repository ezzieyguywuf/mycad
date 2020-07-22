{-|
Module      : Topology
Description : Adjacency relationships for geometric data.
Copyright   : (c) Wolfgang E. Sanyer, 2020
License     : MPL2
Maintainer  : WolfgangESanyer@Gmail.com
Stability   : experimental
Portability : POSIX

This module describes a 'Topology' which can be used to manage and introspect the
relationship between geometric entities, specifically Vertices, Edges, and Faces. The
Topology itself is not aware of any geometry - you could use this to model any similar
relationships.

It was written, however, with the intent it be used in a CAD system.
-}
module Topology
( -- * Data Types
  -- | Constructors for these are not exported, therefore you must use the
  --   functions herein to create any of these
  Topology
, TopoState
  -- ** Topological Entities
  -- | These types can be used to hold a reference to the given topological entity.
  --
  --   Although each of these is a shallow wrapper around an "Int", they have
  --   been defined as "newtype" in order to allow for type-checking
, Vertex
, Face
, Edge
  -- * Mutating
, emptyTopology
, addFreeVertex
, addEdge
, removeVertex
, removeEdge
  -- * Inspecting
, getVertices
, getEdges
, getFaces
)where

import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Test.QuickCheck (Arbitrary, arbitrary, elements)
import Control.Monad.State as St

-- ===========================================================================
--                               Data Types
-- ===========================================================================
-- | A Topology is simply a Graph
--
--   The API defined in this module will control how this Graph can be
--   modified. Any Topological Entity (Vertex, Edge, etc.) will be represented
--   by a Node in this Graph. The relationships between these entities will be
--   defined as the \"Edge\"s between these Nodes. We've decided to call these
--   \"Edge\" \"Bridges\" in order to minimize confusion.
newtype Topology = Topology {unTopology :: TopoGraph}
                   deriving (Show, Eq)

type TopoGraph = Gr NodeLabel BridgeLabel

type TopoState a = St.State Topology a

newtype Vertex = Vertex {getVertexID :: Int} deriving (Show, Eq, Ord)
newtype Edge   = Edge   {getEdgeID   :: Int} deriving (Show, Eq, Ord)
newtype Face   = Face   {getFaceID   :: Int} deriving (Show, Eq)

-- | In fgl, each Node and Bridge in the Graph can contain an arbitrary piece
--   of data. This data is referred to as a \"Label\", and thus we have "LNode"
--   (labelled node) and "LEdge" (labelled edge, which we call a bridge)
--
--   For our Nodes, our data contains an "Element" type, identifying it as a
--   Vertex, Edge, or Face, as well as an integer specifying which n of that
--   data type it is. For example, `NodeLabel (VertexElement 2)` means that this
--   Node is the second Vertex in the Graph
--
--   Although fgl does maintain its own "Int" identifier for each node
--   behind-the-scenes, we still carry along our own, so that we can
--   independently enumerate our different topological entities. Otherwise, if
--   we did someting like add two vertices and then put an edge between them,
--   the edge would be numbered "3" since it is the third node in the graph,
--   and this is not what we want.
data NodeLabel = NodeLabel { getElement   :: Element
                           , getElementID :: Int
                           } deriving (Show, Eq)

-- | These are the fundamental topological entities that our graph will be
--   built from
data Element = VertexElement
             | EdgeElement
             | FaceElement
               deriving (Show, Eq)

-- | Our Bridge can also contain a piece of data, but we don't need any data
--   here so we juset it to ()
newtype BridgeLabel = BridgeLabel () deriving (Show, Ord, Eq)

-- ===========================================================================
--                               Free Functions
-- ===========================================================================
-- | The EmptyTopology is the starting point for any 'Topology'. As no
--   constructors for 'Topology' are exported, this is the only way to create
--   one.
emptyTopology :: Topology
emptyTopology = Topology Graph.empty

-- | Adds a single "free" Vertex to the 'Topology'. In this context, "free"
--   means that it is does not have any entities adjacent to it.
addFreeVertex :: TopoState Vertex
addFreeVertex = addNode VertexElement >>= pure . Vertex

-- | If the Vertex does not exist, this does nothing
removeVertex :: Vertex -> TopoState ()
removeVertex (Vertex n) = do
    topology <- gets unTopology
    put $ Topology $ Graph.delNode n topology
    pure ()

-- | Adds an Edge adjacent to both Vertex
addEdge :: Vertex -> Vertex -> TopoState Edge
addEdge _ _ = undefined

-- | If the Edge does not exist, does nothing.
removeEdge :: Edge -> TopoState ()
removeEdge _ = undefined
    --t <- gets unTopology
    --put (Topology $ Graph.delNode n t)
    --pure ()

-- | Returns all the 'Vertex' in the 'Topology'
getVertices :: Topology -> [Vertex]
getVertices _ = undefined

-- | Returns all the 'Edge' in the 'Topology'
getEdges :: Topology -> [Edge]
getEdges _ = undefined

-- | Returns all the 'Face' in the 'Topology'
getFaces :: Topology -> [Face]
getFaces _ = undefined

-- ===========================================================================
--                        Private Free Functions
-- ===========================================================================
-- | The 'Node' is the basic building block of a 'Graph'. Any given Node can
--   have zero or more adjacencies.
--
--   Note that any Node that is added has two distinct identifiers:
--
--       1. The ID used in the "Graph", GID
--       2. The ID stored in the "Label", LID
--
--  The GID is simply incremented for every new node. So, if you first add two
--  vertices and then an edge, the edge's GID will be 3
--
--  The LID is only incremented when an item of the same type is added, so in
--  this same example, the two vertices and edge will have LID of 0, 1, and 0
--  respectively.
addNode :: Element -> TopoState Int
addNode element = do
    gid <- newGID
    label <- newLabel element
    modify (Topology . Graph.insNode (gid, label) . unTopology)
    pure gid

-- | The GID is the Graph IDentifier, which is used to uniquely identify each
--   node in the data graph. This is required by the fgl library.
newGID :: TopoState Int
newGID = gets $ length . Graph.nodes . unTopology

-- | The label is the data "payload" that our node will cary.
--
--   Currently, it only includes a counter for the element type, so that we
--   know which was the first, second, third, etc. of the "Element" added to
--   the graph
newLabel :: Element -> TopoState NodeLabel
newLabel element = newEID element >>= pure . NodeLabel element

-- | The EID is the Element IDentifier, and it specifies which of the given
--   Element a particular node is.
--
--   For example, an EID of 2 for a "VertexElement" means it is the second
--   "Vertex" in the graph
newEID :: Element -> TopoState Int
newEID element = filterGraph element >>= pure . length . Graph.nodes

-- | Returns a sub-graph in which the nodes are all of the given "Element" type
filterGraph :: Element -> TopoState TopoGraph
filterGraph element = gets $ Graph.labfilter (isElement element) . unTopology

-- | Checks if a particular NodeLabel is of the "Element" type
isElement :: Element -> NodeLabel -> Bool
isElement element label = element == (getElement label)

-- ===========================================================================
--                            Instances
-- ===========================================================================
instance Arbitrary Topology where
    arbitrary = elements [t0, t1, t2]
        where t0 = emptyTopology
              t1 = execState addFreeVertex emptyTopology -- Single free vertex
              t2 = execState addFreeVertex t1            -- Two free vertex
