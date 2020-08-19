{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
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
  -- * Adjacency information
  -- | This is really the heart of this module. It's kind of the whole "point"
  --   of Topology
, vertexEdges
, edgeVertices
, unAdjacency
, wireEdges
  -- | Get information about the topology
, getVertices
, getEdges
, getWire
  -- * Serialization
, vertexID
, vertexFromID
 , edgeID
)where

-- Base
import Data.Maybe (mapMaybe)
import Control.Exception (PatternMatchFail(..), throw)
import Control.Monad (void, unless)
import Data.List ((\\), intersect)

-- third-party
import qualified Data.Set.NonEmpty as NES
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Control.Monad.Except (MonadError, runExceptT, throwError)
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

-- | A Wire is a contiguous series of Edges.
--
--   In this context, "contiguous" means that each Edge has a distinct "left"
--   and "right" Vertex, and the the "left" Vertex of a given Edge in the Wire
--   is always the "right" Vertex of another Edge *unless* the given Edge is
--   the first or last.
--
--   In other words, a Wire looks like: v0 → Edge0 → v1 → Edge1 → v2 →
--   ...→EdgeN → vn.
--
--   Notice that if v0 == vn, then we have a ClosedLoop, otherwise we have an
--   OpenLoop
data Wire = Wire { getFirstVertex :: Vertex
                 , getFirstEdge   :: Edge
                 }
            deriving (Show, Eq)

data Loop = OpenLoop | ClosedLoop deriving (Show, Eq)

-- | Specifies a given pair of topological entities are related to each other
--
--   This is parametrized over the type of "Entity2".
data Adjacency a = In    a -- ^ Entity1 ← Entity2, from Entity2 to Entity1
                 | Out   a -- ^ Entity1 → Entity2, from Entity1 to Entity2
                 | InOut a -- ^ Entity1 ↔ Entity2, both directions between the two
                 deriving (Show, Eq, Functor)

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
addFreeVertex = Vertex <$> addNode VertexEntity

-- | If the Vertex does not exist, this does nothing.
removeVertex :: Vertex -> TopoState ()
removeVertex = void . deleteNode . getVertexID

-- | Adds an Edge adjacent to both Vertex
--
--   Returns Nothing if either Vertex is not already part of the Topology
--
--   Only adds an Edge if there is not already a "v1 → Edge → v2". If there is
--   already an Edge, that same Edge is returned (i.e. the Topology is not
--   modified)
addEdge :: Vertex -> Vertex -> TopoState (Maybe Edge)
addEdge v1 v2 = runMaybeT $ do
    gid1 <- MaybeT (getVertexNode v1)
    gid2 <- MaybeT (getVertexNode v2)
    -- Check if there's already an Edge between the two
    v1Edges <- lift (vertexEdges v1)
    v2Edges <- lift (vertexEdges v2)
    let -- We don't need the adjacency information for this check
        v1Edges' = fmap unAdjacency v1Edges
        v2Edges' = fmap unAdjacency v2Edges
        overlap  = v1Edges' `intersect` v2Edges'
    case overlap of
        [edge] -> pure edge
        _      -> do edge <- lift (addNode EdgeEntity)
                     lift (connectNode gid1 edge)
                     lift (connectNode edge gid2)
                     pure $ Edge edge

-- | This is useful when you don't need to know the Adjacency information
unAdjacency :: Adjacency a -> a
unAdjacency adjacency = case adjacency of
                            In val    -> val
                            Out val   -> val
                            InOut val -> val

-- | Returns the list of Edges that make up the Wire
wireEdges :: Wire -> TopoState (NES.NESet Edge)
wireEdges wire = traverseWire forwardWire (NES.singleton (getFirstEdge wire)) wire

forwardWire :: Eq a => a -> Adjacency a -> Maybe a
forwardWire b a | Out a'  <- a = if a' == b
                                    then Nothing
                                    else Just a'
                | otherwise    = Nothing

traverseWire :: (forall a . Eq a => a ->
                Adjacency a
                -> Maybe a)   -- ^ Used to find the "next" Vertex→Edge pair
            -> NES.NESet Edge -- ^ The cumulative list of found Edges
            -> Wire           -- ^ The current Vertex→Edge pair under inspection
            -> TopoState (NES.NESet Edge)
traverseWire nextFilter edges (Wire vertex edge) = do
    outVertices <- mapMaybe (nextFilter vertex :: Adjacency Vertex -> Maybe Vertex) <$> edgeVertices edge
        :: TopoState [Vertex]

    -- Get all the Out Edges for each Out Vertex
    outEdges <- mapMaybe (nextFilter edge :: Adjacency Edge -> Maybe Edge) . concat
                <$> sequence (vertexEdges <$> outVertices)
        :: TopoState [Edge]

    -- Figure out what to do next
    case (outVertices, outEdges) of
        -- Bail out if we've found a duplicate
        ([outVertex], [outEdge]) ->
            if NES.member outEdge edges
               then pure edges
               else traverseWire nextFilter (NES.insert outEdge edges) (Wire outVertex outEdge)
        -- Bail out if we've reached the end of the chain
        ([_], [])  -> pure edges
        (_:_ , [])  -> throw (PatternMatchFail
                           "A Vertex should have only a single Out Edge")
        ([], []) -> throw (PatternMatchFail
                        "Both Vertex and Edge should not have zero \
                        \Out Adjacencies")
        ([], _:_)  -> throw (PatternMatchFail
                        "An Edge should have one or zero Out Vertices")
        (_:_, _:_) -> throw (PatternMatchFail
                        "Both Vertex and Edge had too many Out adjacencies")

-- | Returns all the Vertices in the Topology, in on particular order
getVertices :: TopoState [Vertex]
getVertices = gets (fmap Vertex . filterNodes VertexEntity . unTopology)

getEdges :: TopoState [Edge]
getEdges = gets (fmap Edge . filterNodes EdgeEntity . unTopology)

-- | If the Edge does not exist, does nothing.
removeEdge :: Edge -> TopoState ()
removeEdge = void . deleteNode . getEdgeID

-- | Returns a (Right Wire) if the Vertex and Edge form a pair such that
--   Vertex→Edge
--
--   The Wire can either be OpenLoop, in which case the first and last Vertex
--   are not the same, or ClosedLoop, in which case it loops all the way back
--   to its starting point
getWire :: Vertex -> Edge -> TopoState (Either String Wire)
getWire vertex edge = runExceptT $ do
    -- Get the Edges adjacent to our Vertex
    adjacentEdges <- lift (vertexEdges vertex)

    -- If the given Edge is not Out from the Vertex, bail out
    unless (elem (Out edge) adjacentEdges) (throwError
        "The given Edge _must_ be an Out Edge of the given Vertex")

    pure (Wire vertex edge)

-- | Returns a list of Edges that are adjacent to the given Vertex
vertexEdges :: Vertex -> TopoState [Adjacency Edge]
vertexEdges (Vertex gid) = fmap (fmap Edge) <$> adjacencies gid EdgeEntity

-- | Returns the list ef Vertices that are adjacent to the given Edge
edgeVertices :: Edge -> TopoState [Adjacency Vertex]
edgeVertices (Edge gid) = fmap (fmap Vertex) <$> adjacencies gid VertexEntity

-- | Returns an Int ID that can be used to re-create the Vertex
vertexID :: Vertex -> TopoState (Maybe Int)
vertexID (Vertex gid) = runMaybeT $ do
    label <- MaybeT (gets ((`lab` gid) . unTopology))
    pure (getEntityID label)

-- | Re-creates a Vertex from the given Int
vertexFromID :: Int -> TopoState (Maybe Vertex)
vertexFromID eid = do
    -- First, make a NodeLabel
    let node = NodeLabel VertexEntity eid

    -- Filter the graph using that NodeLabel
    gids <- gets (nodes . labfilter (node ==) . unTopology)
        :: TopoState [Int]

    -- Filter the TopoGraph
    case gids of
        [gid] -> pure . Just $ (Vertex gid)
        _     -> pure Nothing

-- | Returns an Int ID that can be used to re-create the Edge
edgeID :: Edge -> TopoState (Maybe Int)
edgeID (Edge gid) = runMaybeT $ do
    label <- MaybeT (gets ((`lab` gid) . unTopology))
    pure (getEntityID label)

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
        eid   = length $ filterNodes entity graph
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
    if gelem gid graph
       then pure . Just $ gid
       else pure Nothing

-- | Returns Just the Node in our Graph, if it exists
_getEdgeNode :: Edge -> TopoState (Maybe Int)
_getEdgeNode (Edge gid) = do
    graph <- gets unTopology
    if gelem gid graph
       then pure . Just $ gid
       else pure Nothing

-- | Returns all the Nodes in the graph of the given EntityType
filterNodes :: EntityType -> TopoGraph -> [Int]
filterNodes entity graph = nodes (labfilter predicate graph)
    where predicate = (entity ==) . getEntityType

-- | A helper that returns all adjacent entities of the given type
adjacencies :: Int        -- ^ The GID of the Node in question
            -> EntityType -- ^ The type of the adjacent entities to check
            -> TopoState [Adjacency Int]
adjacencies gid etype = do
    -- first, unwrap the graph from the Topology data type
    graph <- gets unTopology

    let -- create a sub-graph of the entities "In" from our target
        preGraph = subgraph (pre graph gid) graph
        -- create a sub-graph of the entities "Out" from our target
        sucGraph = subgraph (suc graph gid) graph
        -- Create a list of Nodes of the given EntityType for each sub-graph
        inIDs  = filterNodes etype preGraph
        outIDs = filterNodes etype sucGraph
        -- Figure out which are both In and Out
        inoutIDs = inIDs `intersect` outIDs
        -- Filter out InOut values from the separate In and Out lists
        inIDs'  = inIDs \\ inoutIDs
        outIDs' = outIDs \\ inoutIDs
        -- Wrap our GID's in the appropriate Adjacency, as well as Edge data types
        allAdjacencies = fmap In inIDs'
                         <> fmap Out outIDs'
                         <> fmap InOut inoutIDs
    pure allAdjacencies

-- | Turns a `Maybe a` into in `ExceptT e a`, where `e` is the error provided.
_note :: MonadError e m => e -> Maybe a -> m a
_note msg = maybe (throwError msg) pure

