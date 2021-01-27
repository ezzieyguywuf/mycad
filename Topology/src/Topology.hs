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
, faceVertices
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
import Control.Monad (void, when)
import Data.List (intersect)
import Data.Foldable (find)

-- third-party
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State (State, get, put, gets)
import Control.Monad.Except ( MonadError, ExceptT(ExceptT), runExceptT
                            , throwError, liftEither)
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
type TopoState = State Topology

-- | The Link is what holds the relatiosnhip information between topological
--   entities.
--
--   Consider a basic line segment to begin - this consists of a single Edge
--   with a Vertex on each end. In this case, two links would be used to
--   describe the topological relationship:
--
--   Vertex₁ ↔ Link₁ ↔ Edge ↔ Link₂ ↔ Vertex₂
--
--   Notice that neither Vertices know anything about the Edge, and vice-versa.
--   Rather, each topological entity is aware of a Link, which can later be used
--   to figure out what is "adjacent" to each other.
--
--   In this case, both Link₁ and Link₂ would be referred to as "End" Links -
--   this is the most basic form of a Link, which at the very least must refer
--   to a single "parent" Vertex and a single "parent" Edge.
--
--   Let's build on this example and add a second Edge into the mix (we'll
--   abbreviate Vertex to V, Link to L, etc...):
--
--   V₁ ↔ L₁ ↔ E₁ ↔ L₂ ↔ V₂
--   V₂ ↔ L₃ ↔ E₂ ↔ L₄ ↔ V₃
--
--   So, these two Edges are both "adjacent" to V₂, but there is currently no
--   relationship between E₁ and E₂. Why would we need this relationship? Well,
--   that should hopefully become apparent a bit later. For now, let's just add
--   the relationship:
--
--   V₁ ↔ L₁ ↔ E₁ ↔ L₂ ↔ V₂
--   V₂ ↔ L₃ ↔ E₂ ↔ L₄ ↔ V₃
--   L₂ → L₃
--
--   Two things to note here:
--
--   1. L₂ is no longer a simple "End" Link, but rather we can refer to it
--      now as a "Chain" link, because it describes an Edge "chain" from E₁ to
--      E₂
--   2. The relationship here is directional - in other words, L₂ doesn't
--      actually "know" anything about L₁. L₂ is still technically a simple
--      "End" Link
--
--   This is nice, so now we have a "chain" from E₁ to E₂. We can express that
--   visually something like this...
--
--                  V₂   V₂
--                  ↕    ↕
--   V₁ ↔ L₁ ↔ E₁ ↔ L₂ → L₃ ↔ E₂ ↔ L₄ ↔ V₃
--
--   Or simplify it down to:
--
--   V₁ ↔ E₁ ↔ V₂ ↔ E₂ ↔ V₃
--
--   It's important to note that while this final, simplified visual omits the
--   links, that the links still exist and are an essential component of the
--   topological data structure.
--
--   The "chain" link can be used to chain together an arbitrary number of
--   Edges, even looping all the way back to the original link to form a loop.
--
--   It's important to know (this is why I'm mentioning it a third time) that
--   the chains are directional - this is used to establish a "counterclockwise"
--   convention for looping a chain of Edges around a Face.
data Link = Link { getLinkBase  :: LinkBase
                 , getLinkType   :: LinkType
                 }
          deriving (Show, Eq, Ord)

-- | This is the most fundaental information that all links must have
--
--   Each link will have, at the very least, an associated Vertex and Edge, thus
--   we refer to this as a "Vertex-Link" structure.
--
--   The reason we call this the "Base" is because any link relationship *must*
--   include at least these two pieces of information:
--
--      1. what is my "parent" Vertex
--      2. what is my "parent" Edge
--
--   This will fully define the given Link as being "located" (if you will)
--   between the given Vertex and Edge. To think of it differently, the given
--   Link can be considered to "glue" the Vertex and Edge together.
--
--   Any other data, say "what is the next Link in this loop?" can be considered
--   "extra", as it is not strictly needed to fully define the Link, but rather
--   helps to further specialize the type of Link that is being described.
data LinkBase = LinkBase { getLinkVertex :: NodeID
                         , getLinkEdge   :: NodeID
                         } deriving (Show, Eq, Ord)

-- | This extra data is useful for defining different types of links
data LinkType =
      EndLink
    | LoopLink { getNextLink :: LinkBase
               , getLinkFace :: Maybe NodeID }
    deriving (Show, Eq, Ord)

-- | A Vertex can contain zero or more "Link"
newtype TopoVertex = TopoVertex (Set.Set Link) deriving (Show, Eq, Ord)

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
        newVertex = TopoVertex Set.empty

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
    case leftEdges `intersect` rightEdges of
        [edge] -> ExceptT (pure $ Right edge)
        _ -> do
            -- Unpack the topology data
            (Topology vertices edges faces) <- lift get

            -- Get the Vertex data
            (TopoVertex leftLinkSet)  <- ExceptT (lookupVertex v1)
            (TopoVertex rightLinkSet) <- ExceptT (lookupVertex v2)

            -- create the two new links and update our maps
            let leftLink  = Link (LinkBase leftVID  newEdgeID) EndLink
                rightLink = Link (LinkBase rightVID newEdgeID) EndLink
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

-- | Will create a Face from the list of Edges
--
--   Each consecutive Edge must share a common Vertex, with the last Edge
--   closing a loop with the first Edge.
--
--   In other words: V₀ → E₀ → ... Vₙ → Eₙ → V₀
addFace :: [Edge] -> TopoState (Either String Face)
addFace edgeLoop = runExceptT $ do
    -- Make sure we have enough Edges
    when (length edgeLoop < 3)
         (throwError "A minimum of three edges is needed to create a Face")

    -- Ensure it's a closed loop
    let firstEdge = head edgeLoop
        lastEdge  = last edgeLoop
    (TopoVertex startLinks) <- ExceptT (sharesVertex firstEdge lastEdge)

    -- Make pairs of each consective Edge
    let pairs = zip edgeLoop (tail edgeLoop)

    -- Ensure each pair shairs a common Vertex
    lift (mapM (uncurry sharesVertex) pairs)

    -- Unpack the topology data
    (Topology vertices edges faces) <- lift get

    -- Find the target Link, i.e. the first Link in the Face
    firstTopoEdge <- ExceptT (lookupEdge firstEdge)
    targetLink <- liftEither $ case find (edgeHasLink firstTopoEdge) startLinks of
                      Just foundLink -> Right foundLink
                      Nothing -> Left "Could not find a Link to the first \
                                       \Edge from the first Vertex"
    -- Add a face
    let faces'  = Map.insert nFaces newFace faces
        nFaces  = length faces
        newFace = TopoFace targetLink

    -- Write out the updated Topology State
    lift . put $ Topology vertices edges faces'

    -- Return the newly created Face
    pure (Face nFaces)

-- | The inverse of addFace
removeFace :: Face -> TopoState ()
removeFace (Face faceNode) = do
    -- Unpack the topology data
    (Topology vertices edges faces) <- get

    -- delete the appropriate Face
    let faces' = Map.delete faceNode faces

    -- Update the topology data
    put (Topology vertices edges faces')

-- | Returns all the Vertices in the Topology, in on particular order
getVertices :: TopoState [Vertex]
getVertices = fmap Vertex <$> gets (Map.keys . getTopoVertices)

getEdges :: TopoState [Edge]
getEdges = fmap Edge <$> gets (Map.keys . getTopoEdges)

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
vertexEdges vertex =
    lookupVertex vertex >>=
    either (pure . const []) (pure . Set.toList . processVertex)
        where processVertex (TopoVertex links) = Set.map findLinkEdge links

-- | Returns the list ef Vertices that are adjacent to the given Edge
edgeVertices :: Edge -> TopoState [Vertex]
edgeVertices edge =
    lookupEdge edge >>=
    either (pure . const []) (pure . processEdge)
        where processEdge (TopoEdge leftLink rightLink) = [ findLinkVertex leftLink
                                                          , findLinkVertex rightLink
                                                          ]

-- | Returns the list of Vertices that are adjacent to the given Face
faceVertices :: Face -> TopoState (Either String [Vertex])
faceVertices face = runExceptT $ do
    -- First, get the TopoFace
    (TopoFace startLink) <- ExceptT (lookupFace face)

    -- Now, traverse the loop and collect the Vertices
    ExceptT (traverseLoop [] startLink)

-- | Recursively follows the "next" Edge in a chain
--
--   Returns Right [Vertices] if the loop actually loops, or Left String if the
--   loop is open
traverseLoop :: [Vertex] -> Link -> TopoState (Either String [Vertex])
traverseLoop _vertices (Link _ EndLink) = pure (Left "A Loop cannot contain an EndLink")
traverseLoop _vertices (Link _linkBase _linkType) = undefined

-- | Returns an Int ID that can be used to re-create the Vertex
vertexID :: Vertex -> TopoState (Maybe Int)
vertexID (Vertex vid) = bool Nothing (Just vid) <$>
                        gets (Map.member vid . getTopoVertices)

-- | Re-creates a Vertex from the given Int
vertexFromID :: Int -> TopoState (Maybe Vertex)
vertexFromID vid = pure . Just $ Vertex vid

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

lookupVertex :: Vertex -> TopoState (Either String TopoVertex)
lookupVertex (Vertex nodeID) = lookupEither nodeID "Vertex" getTopoVertices

lookupEdge :: Edge -> TopoState (Either String TopoEdge)
lookupEdge (Edge nodeID) = lookupEither nodeID "Vertex" getTopoEdges

lookupFace :: Face -> TopoState (Either String TopoFace)
lookupFace (Face nodeID) = lookupEither nodeID "Face" getTopoFaces

-- | Returns the Vertex that is associated with the Link
findLinkVertex :: Link -> Vertex
findLinkVertex = Vertex . getLinkVertex . getLinkBase

-- | Returns the Edge that is associated with the Link
findLinkEdge :: Link -> Edge
findLinkEdge = Edge . getLinkEdge . getLinkBase

-- | Checks if the TopoEdge contains the given Link
edgeHasLink :: TopoEdge -> Link -> Bool
edgeHasLink (TopoEdge link1 link2) checkLink = checkLink `elem` [link1, link2]

-- | Determines whether or not two Edges have a common Vertex
sharesVertex :: Edge -> Edge -> TopoState (Either String TopoVertex)
sharesVertex leftEdge rightEdge = do
    -- First, get the vertices for both Edges
    leftVertices  <- edgeVertices leftEdge
    rightVertices <- edgeVertices rightEdge

    -- See if they have a single common vertex
    case leftVertices `intersect` rightVertices of
        [commonVertex] -> lookupVertex commonVertex
        []             -> pure . Left $ "The Edges do not share a Vertex"
        _              -> pure . Left $ "The Edges share more than one Vertex"
