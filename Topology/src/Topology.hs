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
, Ray
, Wire
  -- * Mutating
, emptyTopology
, addFreeVertex
, addEdge
, removeVertex
, removeEdge
, makeFace
, removeFace
  -- * Adjacency information
  -- | This is really the heart of this module. It's kind of the whole "point"
  --   of Topology
, vertexEdges
, edgeVertices
  -- | Get information about the topology
, getVertices
, getEdges
, getRay
, getWire
  -- * Serialization
, vertexID
, vertexFromID
 , edgeID
)where

-- Base
import Control.Monad (void)
import Data.List (intersect)

-- third-party
import qualified Data.Set.NonEmpty as NES
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Control.Monad.Except (ExceptT(ExceptT), MonadError, runExceptT, throwError)
import Control.Monad.State (State, gets, modify)
import Data.Graph.Inductive.Graph (empty, delNode, insNode, nodes, labfilter
                                  , gelem, insEdge, lab, subgraph
                                  , neighbors)
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

-- | A Ray is a Vertex/Edge pair.
--
--   Speifically, a set of relationships exists such that:
--
--       - the Vertex has `Out Edge` as an adjacency
--       - the Edge has `In Vertx` as an adjacency.
--
--   In other words, a Ray is a Vertex→Edge
data Ray = Ray Vertex Edge
            deriving (Show)

-- | A set of contiguous Rays
--
--   In this context, "contiguous" means that there is a unbroken chain of
--   Vertex→Edge pairs
data Wire = OpenWire EdgeSet
          | ClosedWire EdgeSet
          deriving (Show, Eq)

-- | A Non-Empty (unique) Set of Edges
type EdgeSet = NES.NESet Edge

-- | Specifies a given pair of topological entities are related to each other
--
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
                | LinkEntity
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
addEdge :: Vertex -> Vertex -> TopoState (Either String Edge)
addEdge vertex1 vertex2 = runExceptT $ do
    -- Make sure both GID's exist in our graph
    v1 <- ExceptT (getVertexNode vertex1)
    v2 <- ExceptT (getVertexNode vertex2)

    -- Check if there's already an Edge between the two
    v1Edges <- lift (vertexEdges vertex1)
    v2Edges <- lift (vertexEdges vertex2)
    case v1Edges `intersect` v2Edges of
        [edge] -> pure edge
        _      -> do -- Add two new nodes
                     link1 <- lift (addNode LinkEntity)
                     link2 <- lift (addNode LinkEntity)
                     edge  <- lift (addNode EdgeEntity)

                     -- A vertex must know about all it's links
                     lift (connectNode v1 link1)
                     lift (connectNode v2 link2)

                     -- Each link must know about which vertex it belongs to
                     lift (connectNode link1 v1)
                     lift (connectNode link2 v2)

                     -- Each link must know which edge it is connected to
                     lift (connectNode link1 edge)
                     lift (connectNode link2 edge)

                     -- Each edge must know about its links
                     lift (connectNode edge link1)
                     lift (connectNode edge link2)
                     pure $ Edge edge

-- | Will create a face from a ClosedWire. Returns an error if OpenWire
makeFace :: Wire -> TopoState (Either String Face)
makeFace (OpenWire _)   = pure (Left "A Face can only be made with an OpenWire")
makeFace (ClosedWire _) = undefined

-- | The inverse of makeFace
removeFace :: Face -> TopoState ()
removeFace _ = pure ()

-- | Returns the list of Edges that make up the Wire
getWire :: Ray -> TopoState Wire
getWire startRay@(Ray _ startEdge) = do
    -- The initial set of Edges
    let startSet = NES.singleton startEdge

    -- Cycle Forward
    (_, forwardSet) <- gets (goForward startRay startSet)

    -- Cycle Backwards
    (looped, finalSet) <- gets (goBackward startRay forwardSet)

    if looped
       then pure (ClosedWire finalSet)
       else pure (OpenWire finalSet)

-- | Returns all the Vertices in the Topology, in on particular order
getVertices :: TopoState [Vertex]
getVertices = gets (fmap Vertex . filterNodes VertexEntity . unTopology)

getEdges :: TopoState [Edge]
getEdges = gets (fmap Edge . filterNodes EdgeEntity . unTopology)

-- | If the Edge does not exist, does nothing.
removeEdge :: Edge -> TopoState ()
removeEdge (Edge eid) = do
    -- Get the Edge's links
    linkIDs <- adjacencies LinkEntity eid

    -- Delete all the pertinent nodes
    sequence_ (fmap deleteNode (eid : linkIDs))

-- | Returns a (Right Ray) if the Vertex and Edge form a pair such that
--   Vertex→Edge
getRay :: Vertex -> Edge -> TopoState (Either String Ray)
getRay _ _ = undefined
--getRay vertex edge = runExceptT $ do
    ---- Get the Edges adjacent to our Vertex
    --adjacentEdges <- lift (vertexEdges vertex)

    ---- If the given Edge is not Out from the Vertex, bail out
    --unless (elem (Out edge) adjacentEdges) (throwError
        --"The given Edge _must_ be an Out Edge of the given Vertex")

    --pure (Ray vertex edge)

-- | Returns a list of Edges that are adjacent to the given Vertex
vertexEdges :: Vertex -> TopoState [Edge]
vertexEdges (Vertex vid) = do
    -- First get all the links that the Vertex knows about
    linkIDs <- adjacencies LinkEntity vid

    -- Collect all the Edges attached to the links
    edgeIDs <- concat <$> sequence (fmap (adjacencies EdgeEntity) linkIDs)

    -- Return the list of Edges
    pure (fmap Edge edgeIDs)

-- | Returns the list ef Vertices that are adjacent to the given Edge
edgeVertices :: Edge -> TopoState [Vertex]
edgeVertices (Edge eid) = do
    -- First get all the links that the Edge knows about
    linkIDs <- adjacencies LinkEntity eid

    -- Collect all the Vertices attached to the links
    vertexIDs <- concat <$> sequence (fmap (adjacencies VertexEntity) linkIDs)

    -- Return the list of Vertices
    pure (fmap Vertex vertexIDs)

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

-- | Returns `Right gid` if the node exists, otherwise an error
getVertexNode :: Vertex -> TopoState (Either String Int)
getVertexNode (Vertex gid) = do
    graph <- gets unTopology
    if gelem gid graph
       then pure . Right $ gid
       else pure (Left ("The gid \"" <> show gid <> "\" does not exist in the graph"))

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
adjacencies :: EntityType -- ^ The type of the adjacent entities to check
            -> Int        -- ^ The GID of the Node in question
            -> TopoState [Int]
adjacencies etype gid = do
    -- First, get all adjacent nodes
    gids <- gets (flip neighbors gid . unTopology)

    -- Make a sub graph of just these adjacent nodes
    graph <- gets (subgraph gids . unTopology)

    -- Return the nodes in the sub-graph that match
    pure (filterNodes etype graph)

-- | Turns a `Maybe a` into in `ExceptT e a`, where `e` is the error provided.
_note :: MonadError e m => e -> Maybe a -> m a
_note msg = maybe (throwError msg) pure

goForward :: Ray -> NES.NESet Edge -> Topology -> (Bool, EdgeSet)
goForward _ _ _= undefined
--goForward (Ray _ edge) currentSet topology =
    --case (outVertices, outEdges) of
        --([outVertex], [outEdge]) ->
            --if outEdge `NES.notMember` currentSet
               --then goForward newRay newSet topology
               --else (True, newSet)
                   --where newRay  = Ray outVertex outEdge
                  --newSet  = NES.insert outEdge currentSet
        --_ -> (False, currentSet)
            --where
                ---- Get a list of adjacent Out vertices
        --adjacentVertices = evalState (edgeVertices edge) topology
        --outVertices      = mapMaybe mapOut adjacentVertices
        --mapOut (Out a)   = Just a
        --mapOut _         = Nothing

        ---- Get a list of adjacent Out edges
        --adjacentEdgeState = sequence (map vertexEdges outVertices)
        --adjacentEdges     = concat (evalState adjacentEdgeState topology)
        --outEdges          = mapMaybe mapOut adjacentEdges

goBackward :: Ray -> NES.NESet Edge -> Topology -> (Bool, EdgeSet)
goBackward _ _ _= undefined
--goBackward (Ray vertex _) currentSet topology =
    --case (inVertices, inEdges) of
        --([inVertex], [inEdge]) ->
            --if inEdge `NES.notMember` currentSet
               --then goBackward newRay newSet topology
               --else (True, newSet)
            --where newRay  = Ray inVertex inEdge
                  --newSet  = NES.insert inEdge currentSet
        --_ -> (False, currentSet)
    --where
        ---- Get a list of adjacent In edges
        --adjacentEdges = evalState (vertexEdges vertex) topology
        --inEdges       = mapMaybe mapIn adjacentEdges
        --mapIn (In a)  = Just a
        --mapIn _       = Nothing

        ---- Get a list of adjacent In vertices
        --adjacentVertexState = sequence (map edgeVertices inEdges)
        --adjacentVertices    = concat (evalState adjacentVertexState topology)
        --inVertices          = mapMaybe mapIn adjacentVertices

