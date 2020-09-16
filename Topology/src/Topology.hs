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
  --   been defined as "newtype" in order to allow for type-checking.
  --
  --   Note that the constructors are not exported - these are not intended to
  --   be created ad-hoc, but rather using the API provided herein
, Vertex
, Face
, Edge
  -- * Mutating
, emptyTopology
, addFreeVertex
, addEdge
, addFace
, removeVertex
, removeEdge
, removeFace
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
import Data.Bool (bool)
import Control.Monad (void)
import Data.List (intersect)

-- third-party
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State (State, get, put, gets)
import Control.Monad.Except (MonadError, ExceptT(ExceptT), runExceptT, throwError)
import Control.Monad.Trans.Class (lift)

-- ===========================================================================
--                               Data Types
-- ===========================================================================
-- | A Topology is a collection of relationships between Vertex, Edge, and Face
--
--   The "Link" data type will hold the actual adjacency information, and the
--   Topology data type will mantain a list of all Vertices, Edges, and Faces
data Topology = Topology { getTopoVertices :: Vertices
                         , getTopoEdges    :: Edges
                         , getTopoFaces    :: Faces
                         } deriving (Show, Eq)

-- | They key in our Topology Map.
type NodeID = Int

-- | These three type aliases hold our three topological entities
type Vertices = Map.Map NodeID TopoVertex
type Edges    = Map.Map NodeID TopoEdge
type Faces    = Map.Map NodeID TopoFace

-- | Used to manage the State of the Topology
type TopoState a = State Topology a

-- | The Link is what holds the relatiosnhip information between topological
--   entities
data Link = Link { getBasicLink  :: BasicLink
                 , getLinkFace   :: NodeID
                 , getNextLink   :: Link
                 }
          | LoopLink { getBasicLink :: BasicLink
                     , getNextLink  :: Link
                     }
          | EndLink { getBasicLink :: BasicLink}
          deriving (Show, Eq, Ord)

-- | This is the most fundaental information that all links must have
data BasicLink = BasicLink { getLinkVertex :: NodeID
                           , getLinkEdge   :: NodeID
                           } deriving (Show, Eq, Ord)


-- | A Vertex can contain zero or more "Link"
data TopoVertex = TopoVertex (Set.Set Link) deriving (Show, Eq, Ord)

-- | An Edge must contain exactly two "Link"
data TopoEdge = TopoEdge Link Link deriving (Show, Eq, Ord)

-- | A Face will be defined by a single Link
--
--   This API will guarantee that the Link is always a LoopLink, so that we
--   always have a list of Edges in a Loop that define the Face
newtype TopoFace = TopoFace Link deriving (Show, Eq, Ord)

newtype Vertex = Vertex NodeID deriving (Show, Eq, Ord)
newtype Edge   = Edge   NodeID deriving (Show, Eq, Ord)
newtype Face   = Face   NodeID deriving (Show, Eq, Ord)

-- ===========================================================================
--                               Free Functions
-- ===========================================================================
-- | The EmptyTopology is the starting point for any 'Topology'. As no
--   constructors for 'Topology' are exported, this is the only way to create
--   one.
emptyTopology :: Topology
emptyTopology = Topology Map.empty Map.empty Map.empty

-- | Adds a single "free" Vertex to the 'Topology'. In this context, "free"
--   means that it is does not have any entities adjacent to it.
addFreeVertex :: TopoState Vertex
addFreeVertex = do
    -- Unpack the data in the Topology
    (Topology vertices edges faces) <- get

    -- Calculate our new Vertices
    let vertices' = Map.insert nVertices newVertex vertices
        nVertices = length vertices
        newVertex = TopoVertex (Set.empty)

    -- update our State
    put (Topology vertices' edges faces)

    -- Return the newly created Vertex
    pure (Vertex nVertices)

-- | If the Vertex does not exist, this does nothing.
removeVertex :: Vertex -> TopoState ()
removeVertex (Vertex nodeID) = do
    -- unpack teh data in the Topology
    (Topology vertices edges faces) <- get

    -- delete the appropriate Vertex
    let vertices' = Map.delete nodeID vertices

    -- Update the topology data
    put (Topology vertices' edges faces)

-- | Adds an Edge adjacent to both Vertex
--
--   Returns Nothing if either Vertex is not already part of the Topology
--
--   Only adds an Edge if there is not already a "v1 → Edge → v2". If there is
--   already an Edge, that same Edge is returned (i.e. the Topology is not
--   modified)
addEdge :: Vertex -> Vertex -> TopoState (Either String Edge)
addEdge v1@(Vertex leftVID) v2@(Vertex rightVID) = runExceptT $ do
    -- Bail out if there is already an Edge between these two Vertices
    leftEdges  <- lift (vertexEdges v1)
    rightEdges <- lift (vertexEdges v2)
    case intersect leftEdges rightEdges of
        [edge] -> ExceptT (pure $ Right edge)
        _ -> do
            -- Unpack the topology data
            (Topology vertices edges faces) <- lift get

            -- Get the Vertex data
            (TopoVertex leftLinkSet)  <- ExceptT (lookupVertex leftVID)
            (TopoVertex rightLinkSet) <- ExceptT (lookupVertex rightVID)

            -- create the two new links and update our maps
            let leftLink  = EndLink (BasicLink leftVID  newEdgeID)
                rightLink = EndLink (BasicLink rightVID newEdgeID)
                newEdgeID = length edges
                vertices' = Map.insert leftVID  (insertVertex leftLink leftLinkSet) (
                            Map.insert rightVID (insertVertex rightLink rightLinkSet)
                                vertices)
                insertVertex link links = TopoVertex (Set.insert link links)
                edges'    = Map.insert newEdgeID (TopoEdge leftLink rightLink) edges

            -- Write back our updated data
            put (Topology vertices' edges' faces)

            -- Return the newly created Edge
            pure (Edge newEdgeID)

-- | Will create a Face from an "edge loop"
--
--   Here, "edge loop" is defined as a chain of end-to-end edges that form a
--   closed loop.
--
--   This loop can be identified by a single Vertex and Edge by following the
--   following rule - travelling from the Vertex along the Edge, any time
--   another Vertex is encountered, the next Edge to the "left" is followed.
--   This pattern is repeated until either a dangling Vertex is found or the
--   loop is completed
--
--   Note that "left" here follows the "right-hand rule". In other word, if you
--   curl your right fingers from the Edge you're on towards the next "left"
--   Edge, then your thumb will be pointing upwards, normal from the Face
--   you're tracing
addFace :: [Edge] -> TopoState (Either String Face)
addFace _ = runExceptT $ do
    undefined

-- | The inverse of addFace
removeFace :: Face -> TopoState ()
removeFace _ = pure ()

-- | Returns all the Vertices in the Topology, in on particular order
getVertices :: TopoState [Vertex]
getVertices = gets (Map.keys . getTopoVertices) >>= pure . fmap Vertex

getEdges :: TopoState [Edge]
getEdges = gets (Map.keys . getTopoEdges) >>= pure . fmap Edge

-- | If the Edge does not exist, does nothing.
removeEdge :: Edge -> TopoState ()
removeEdge (Edge eid) = void $ runExceptT $ do
    -- unpack the topology data
    (Topology vertices edges faces) <- lift get

    -- update the edge data and delete orphaned links in vertex data
    let edges' = Map.delete eid edges
        vertices' = Map.map processVertex vertices
        processVertex (TopoVertex links) = TopoVertex (Set.filter filterLink links)
        filterLink link = findLinkEdge link /= Edge eid

    -- write the updated data
    lift (put (Topology vertices' edges' faces))

-- | Returns a list of Edges that are adjacent to the given Vertex
vertexEdges :: Vertex -> TopoState [Edge]
vertexEdges (Vertex vid) =
    lookupVertex vid >>=
    either (pure . const []) (pure . Set.toList . processVertex)
        where processVertex (TopoVertex links) = Set.map findLinkEdge links

-- | Returns the list ef Vertices that are adjacent to the given Edge
edgeVertices :: Edge -> TopoState [Vertex]
edgeVertices (Edge eid) =
    lookupEdge eid >>=
    either (pure . const []) (pure . processEdge)
        where processEdge (TopoEdge leftLink rightLink) = [ findLinkVertex leftLink
                                                          , findLinkVertex rightLink
                                                          ]

-- | Returns an Int ID that can be used to re-create the Vertex
vertexID :: Vertex -> TopoState (Maybe Int)
vertexID (Vertex vid) = gets (Map.member vid . getTopoVertices) >>=
                        pure . bool Nothing (Just vid)

-- | Re-creates a Vertex from the given Int
vertexFromID :: Int -> TopoState (Maybe Vertex)
vertexFromID vid = pure . Just $ (Vertex vid)

-- | Returns an Int ID that can be used to re-create the Edge
edgeID :: Edge -> TopoState (Maybe Int)
edgeID (Edge eid) = pure . Just $ eid

-- ===========================================================================
--                        Private, Non-Exported stuff
-- ===========================================================================
note :: MonadError e m => e -> Maybe a -> m a
note msg = maybe (throwError msg) pure

lookupEither :: (Ord k, Show k)
             => k -> String -> (Topology -> Map.Map k v)
             -> TopoState (Either String v)
lookupEither key topoName getDataMap =
    gets (Map.lookup key . getDataMap) >>=
    runExceptT . note ("The " <> topoName <>
                       " with ID " <> show key <>
                       " does not exist in the topology")

lookupVertex :: NodeID -> TopoState (Either String TopoVertex)
lookupVertex nodeID = lookupEither nodeID "Vertex" getTopoVertices

lookupEdge :: NodeID -> TopoState (Either String TopoEdge)
lookupEdge nodeID = lookupEither nodeID "Vertex" getTopoEdges

-- | Returns the Vertex that is associated with the Link
findLinkVertex :: Link -> Vertex
findLinkVertex = Vertex . getLinkVertex . getBasicLink

-- | Returns the Edge that is associated with the Link
findLinkEdge :: Link -> Edge
findLinkEdge = Edge . getLinkEdge . getBasicLink
