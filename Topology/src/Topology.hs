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
, Adjacency(..)
  -- * Mutating
, emptyTopology
, addFreeVertex
, addEdge
, removeVertex
, removeEdge
  -- * Adjacency information - this is really the heart of this module. It's
  --   kind of the whole "point" of Topology
, vertexEdges
  -- * Serialization
, vertexID
, vertexFromID
)where

-- Base
import Control.Monad (void)

-- third-party
import Control.Monad (mzero)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Control.Monad.State (State, gets, modify)
import Data.Graph.Inductive.Graph (empty, delNode, insNode, nodes, labfilter
                                  , gelem, insEdge, lab, subgraph, suc, pre)
import Data.Graph.Inductive.PatriciaTree (Gr)

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

-- | Used to manage the State of the Topology
type TopoState a = State Topology a

-- | A Vertex can be adjacent:
--
--       - zero or more Edge
--       - zero or more Face
newtype Vertex = Vertex {getVertexID :: Int} deriving (Show, Eq, Ord)
-- | An Edge can be adjacent to:
--
--       - zero, one, or two Vertex
--       - Zero, one, or two Face
newtype Edge   = Edge   {getEdgeID   :: Int} deriving (Show, Eq, Ord)
-- | A Face will be adjacent to at least one Edge, and at least one Vertex
newtype Face   = Face   {getFaceID   :: Int} deriving (Show, Eq)

-- | Specifies a given pair of topological entities are related to each other
--
--   This is parametrized over the type of "Entity2".
data Adjacency a = In    a -- ^ Entity1 ← Entity2, from Entity2 to Entity1
                 | Out   a -- ^ Entity1 → Entity2, from Entity1 to Entity2
                 | InOut a -- ^ Entity1 ↔ Entity2, both directions between the two
                 deriving (Show, Eq)

-- | In fgl, each Node and Bridge in the Graph can contain an arbitrary piece
--   of data. This data is referred to as a \"Label\", and thus we have "LNode"
--   (labelled node) and "LEdge" (labelled edge, which we call a bridge)
--
--   For our Nodes, our data contains an "Entity" type, identifying it as a
--   Vertex, Edge, or Face, as well as an integer specifying which n of that
--   data type it is. For example, `NodeLabel (VertexEntity 2)` means that this
--   Node is the second Vertex in the Graph
--
--   Although fgl does maintain its own "Int" identifier for each node
--   behind-the-scenes, we still carry along our own, so that we can
--   independently enumerate our different topological entities. Otherwise, if
--   we did someting like add two vertices and then put an edge between them,
--   the edge would be numbered "3" since it is the third node in the graph,
--   and this is not what we want.
data NodeLabel = NodeLabel { getEntityType :: EntityType
                           , getEntityID   :: Int
                           } deriving (Show, Eq)

-- | These are the fundamental topological entities that our graph will be
--   built from
data EntityType = VertexEntity
                | EdgeEntity
                | FaceEntity
                  deriving (Show, Eq)

-- | Our Bridge can also contain a piece of data, but we don't need any data
--   here so we juset it to ()
type BridgeLabel = ()

-- ===========================================================================
--                               Free Functions
-- ===========================================================================
-- | The EmptyTopology is the starting point for any 'Topology'. As no
--   constructors for 'Topology' are exported, this is the only way to create
--   one.
emptyTopology :: Topology
emptyTopology = Topology empty

-- | Adds a single "free" Vertex to the 'Topology'. In this context, "free"
--   means that it is does not have any entities adjacent to it.
addFreeVertex :: TopoState Vertex
addFreeVertex = addNode VertexEntity >>= pure . Vertex

-- | If the Vertex does not exist, this does nothing.
removeVertex :: Vertex -> TopoState ()
removeVertex = void . deleteNode . getVertexID

-- | Adds an Edge adjacent to both Vertex
--
--   Returns Nothing if either Vertex is not already part of the Topology
addEdge :: Vertex -> Vertex -> TopoState (Maybe Edge)
addEdge v1 v2 = runMaybeT $ do
    gid1 <- MaybeT (getVertexNode v1)
    gid2 <- MaybeT (getVertexNode v2)
    edge <- lift (addNode EdgeEntity)
    lift (connectNode gid1 edge)
    lift (connectNode edge gid2)
    pure $ Edge edge

-- | If the Edge does not exist, does nothing.
removeEdge :: Edge -> TopoState ()
removeEdge = void . deleteNode . getEdgeID

-- | Returns a list of Edges that are adjacent to the given Vertex
vertexEdges :: Vertex -> TopoState [Adjacency Edge]
vertexEdges (Vertex gid) = do
    outAdjacencies gid EdgeEntity >>= pure . fmap (Out . Edge)

-- | A helper that returns all the outgoing connections from the given GID of
--   the given EntityType, e.g. "GID → Node" would be returned, but "Node →
--   GID" would not be returned
outAdjacencies :: Int -> EntityType -> TopoState [Int]
outAdjacencies gid etype = do
    -- first, unwrap the graph from the Topology data type
    graph <- gets unTopology
    -- next, create a sub-graph of the entities adjacent to the given Vertex
    let graph' = subgraph (suc graph gid) graph
    -- next, filter out just the nodes with the given EntityType and return the result
    pure (nodes $ (labfilter ((etype ==) . getEntityType) graph'))

-- | A helper that returns all the incoming connections to the given GID of
--   the given EntityType, e.g. "GID → Node" not be returned, but "Node →
--   GID" would be returned
_filterIn :: Int -> EntityType -> TopoState [Int]
_filterIn gid etype = do
    -- first, unwrap the graph from the Topology data type
    graph <- gets unTopology
    -- next, create a sub-graph of the entities adjacent to the given Vertex
    let graph' = subgraph (pre graph gid) graph
    -- next, filter out just the nodes with the given EntityType and return the result
    pure (nodes $ (labfilter ((etype ==) . getEntityType) graph'))

-- | Returns an Int ID that can be used to re-create the Vertex
vertexID :: Vertex -> TopoState (Maybe Int)
vertexID (Vertex gid) = runMaybeT $ do
    label <- MaybeT (gets ((`lab` gid) . unTopology))
    pure (getEntityID label)

-- | Re-creates a Vertex from the given Int
vertexFromID :: Int -> TopoState (Maybe Vertex)
vertexFromID eid = runMaybeT $ do
    gids <- lift . gets $ (nodes . labfilter ((eid == ) . getEntityID) . unTopology) 
    case gids of
        [gid] -> pure . Vertex $ gid
        _          -> mzero


-- ===========================================================================
--                        Private, Non-Exported stuff
-- ===========================================================================
-- | The "Graph.Node" is the basic building block of a "Graph.Graph". Any given
--   Node can have zero or more adjacencies.
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
--
--  The return value is the GID
addNode :: EntityType -> TopoState Int
addNode entity = do
    graph <- gets unTopology
    let gid   = length . nodes $ graph
        eid   = length . nodes $ filterGraph entity graph
        label = NodeLabel entity eid
    modify (Topology . insNode (gid, label) . unTopology)
    pure gid

-- | This removes the Node from the Graph.
deleteNode :: Int -> TopoState ()
deleteNode n = modify (Topology . delNode n . unTopology) >> pure ()

-- | Creates a Bridge *from* n1 *to* n2
connectNode :: Int -> Int -> TopoState ()
connectNode n1 n2 = modify (Topology . insEdge (n1, n2, ()) . unTopology)

-- | Returns Just the Node in our Graph, if it exists
getVertexNode :: Vertex -> TopoState (Maybe Int)
getVertexNode (Vertex gid) = do
    graph <- gets unTopology
    case gelem gid graph of
        True  -> pure . Just $ gid
        False -> pure Nothing

-- | Returns a sub-graph in which the nodes are all of the given "Entity" type
filterGraph :: EntityType -> TopoGraph -> TopoGraph
filterGraph entity graph = labfilter predicate graph
    where predicate = (entity ==) . getEntityType
