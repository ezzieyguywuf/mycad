{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
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
--, makeFace
--, removeFace
  -- * Adjacency information
  -- | This is really the heart of this module. It's kind of the whole "point"
  --   of Topology
, vertexEdges
, edgeVertices
  -- | Get information about the topology
, getVertices
, getEdges
  -- * Serialization
, vertexID
, vertexFromID
, edgeID
)where

-- Base

-- third-party
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State (State, gets, modify)

-- ===========================================================================
--                               Data Types
-- ===========================================================================
-- | A Topology is a collection of relationships between Vertex, Edge, and Face
--
--   Really, this forms a sort of "graph", but rather than using (say) the fgl
--   library, we're implementing the data structure ourselves. The reason is
--   that we want to control how each Node in our graph can behave, i.e. how
--   many and what types of connections it can have with other Nodes in the
--   Graph. This control will allow us to simplify the API and logic.
type Topology = Map.Map NodeID TopoEntity

-- | Used to manage the State of the Topology
type TopoState a = State Topology a

-- | They key in our Topology Map.
type NodeID = Int

-- | The "nodes" in our graph.
data TopoEntity = TopoVertex Vertex EntityID
                | TopoEdge   Edge EntityID
                | TopoFace   Face EntityID
                deriving (Show, Eq)

-- | Which nth of the given Entity is it?
--
--   For example, the 1st, 2nd, 3rd, etc.. Vertex would have 0, 1, 2, etc...
--   for EntityID
type EntityID = Int

-- | The Vertex is the backbone of our topological structure - it will be
--   adjacent to zero or more Link
data Vertex = Vertex { getVertexLinks :: LinkSet
                     , getVertexNodeID :: NodeID
                     } deriving (Show, Eq, Ord)

-- | A Link is used to glue together Edges and Vertices, and can also be used
--   to trace a loop around a Face
data Link = Link { getLinkVertex :: NodeID
                 , getLinkEdge   :: NodeID
                 , getNextLinkID :: NodeID
                 } deriving (Show, Eq, Ord)

type LinkSet = Set.Set Link

-- | An Edge will be adjacent to two Link...maybe one for a loop Edge
data Edge = Edge { getLeftLink :: Link
                 , getRightLink :: Link
                 } deriving (Show, Eq, Ord)

-- | A Face will be adjacent to a single Link, which can be used to trace a
--   loop around the Face
newtype Face = Face {getFaceLink :: Link} deriving (Show, Eq)

-- | Used to distinguish the different entity types from each other
data EntityType = VertexEntity | EdgeEntity | FaceEntity deriving (Show)

-- ===========================================================================
--                               Free Functions
-- ===========================================================================
-- | The EmptyTopology is the starting point for any 'Topology'. As no
--   constructors for 'Topology' are exported, this is the only way to create
--   one.
emptyTopology :: Topology
emptyTopology = Map.empty

-- | Adds a single "free" Vertex to the 'Topology'. In this context, "free"
--   means that it is does not have any entities adjacent to it.
addFreeVertex :: TopoState Vertex
addFreeVertex = do
    -- What is the "next" key value?
    key <- gets Map.lookupMax >>=
        \case
             Nothing -> pure 0
             Just (k, _) -> pure (k + 1)
                                
    -- How many Vertices are in the graph right now?
    nVertices <- gets (length . Map.filter (isEntity VertexEntity))

    -- Add the new Vertex to the Map
    let vertex = Vertex (Set.empty) key
    modify (Map.insert key (TopoVertex vertex nVertices))

    -- Return the added vertex
    pure vertex

-- | If the Vertex does not exist, this does nothing.
removeVertex :: Vertex -> TopoState ()
removeVertex = modify . Map.delete . getVertexNodeID

-- | Adds an Edge adjacent to both Vertex
--
--   Returns Nothing if either Vertex is not already part of the Topology
--
--   Only adds an Edge if there is not already a "v1 → Edge → v2". If there is
--   already an Edge, that same Edge is returned (i.e. the Topology is not
--   modified)
addEdge :: Vertex -> Vertex -> TopoState (Either String Edge)
addEdge _ _ = undefined

-- | Will create a face from a ClosedWire. Returns an error if OpenWire
--makeFace :: Wire -> TopoState (Either String Face)
--makeFace (OpenWire _)   = pure (Left "A Face can only be made with an OpenWire")
--makeFace (ClosedWire _) = undefined

-- | The inverse of makeFace
--removeFace :: Face -> TopoState ()
--removeFace _ = pure ()

-- | Returns all the Vertices in the Topology, in on particular order
getVertices :: TopoState [Vertex]
getVertices = undefined

getEdges :: TopoState [Edge]
getEdges = undefined

-- | If the Edge does not exist, does nothing.
removeEdge :: Edge -> TopoState ()
removeEdge _ = undefined

-- | Returns a list of Edges that are adjacent to the given Vertex
vertexEdges :: Vertex -> TopoState [Edge]
vertexEdges _ = undefined

-- | Returns the list ef Vertices that are adjacent to the given Edge
edgeVertices :: Edge -> TopoState [Vertex]
edgeVertices _ = undefined

-- | Returns an Int ID that can be used to re-create the Vertex
vertexID :: Vertex -> TopoState (Maybe Int)
vertexID _ = undefined

-- | Re-creates a Vertex from the given Int
vertexFromID :: Int -> TopoState (Maybe Vertex)
vertexFromID _ = undefined

-- | Returns an Int ID that can be used to re-create the Edge
edgeID :: Edge -> TopoState (Maybe Int)
edgeID _ = undefined

-- ===========================================================================
--                        Private, Non-Exported stuff
-- ===========================================================================
isEntity :: EntityType -> TopoEntity -> Bool
isEntity VertexEntity (TopoVertex _ _)= True
isEntity _ _ = False
