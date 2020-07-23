{-|
Module      : Entity
Description : An Entity is a 3-dimensional object with adjacency knowledge
Copyright   : (c) Wolfgang E. Sanyer, 2020
License     : MPL2
Maintainer  : WolfgangESanyer@Gmail.com
Stability   : experimental
Portability : POSIX

Specifically, we can think of Geometry and Topology as two distinct concepts: whereas
Geometry deals with Points, Curves, and Surfaces in euclidean space, Topology deals with
Vertices, Edges, ad Faces in a "graph".

What does this mean? Well, a piece of Geometry tells you where something is, or how big it
is, or if it intersects with something else

A piece of Topology, on the other hand, tells you whether or not two things are connected,
i.e. "neighbors".

By putting these two concepts together in the form of an Entity, we have the essential
building block for a Computer Aided Design, or CAD, software.

Please note that there could be some confusion between an "Entity.Vertex" and
"Topology.Vertex", and similarly an "Entity.Edge" and a "Topology.Edge". A
distinct decision was made to leave the nomenclature this way - at the end of
the day, there are rather ambiguous definitions for Vertex, Edge, and even
Face. There does not seem to be a clear concensus as to whether these entities
should be strictly topological or include geometric information.V

In the future, it is expected that there will be other "Entity" data types that
far suprass any amount of "Topology" data types available. For example,
"EdgeLoop", "Solid", "ExtrudedSolid", etc... This should make it clear that the
data types in "Topology" are expected to be the very primitive foundations of a
BREP representation, while the data types in "Entity" are intended to build
upon the relational information in "Topology", using "Geometry" as necessary to
make useful data.
-}
module Entity
( -- * Exported Types
  Entity
, EntityState
  -- * Creation and Modification
, nullEntity
, emptyEntityState
, addVertex
, addEdge
  -- * Inspection
, getVertices
, getEdges
, getPoint
, getVertex
, getCurve
, oppositeVertex
  -- * Pretty Printing
, prettyPrintEntity
) where

-- Third-party
import qualified Data.Map as Map
import Data.Text.Prettyprint.Doc (Doc)
import Control.Monad (when, mzero)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Control.Monad.State (State, get, gets, runState, evalState, put)

-- Internal
import qualified Geometry as Geo
import qualified Topology as Topo

-- ===========================================================================
--                               Data Types
-- ===========================================================================
-- | This maps a "Topology.Vertex" to a specfic "Geometry.Point".
type VertexMap p = Map.Map Topo.Vertex (Geo.Point p)

-- | This maps a "Topology.Edge" to a specific "Geometry.Curve"
type EdgeMap p = Map.Map Topo.Edge (Geo.Line p)

-- | The main data type, encapsulating an entire "Topology" and associated
--   "Geometry"
--
--   Please note that the "Vertex" and "Edge" stored here are distinctly *not*
--   "Topology.Vertex" nor "Topology.Edge". Rather, these are primitive
--   "Entity" data types that encapsulate both "Topology" and "Geometry"
data Entity p = Entity { getVertexMap :: VertexMap p
                       , getEdgeMap   :: EdgeMap p
                       , _getTopology :: Topo.Topology
                       } deriving (Show, Eq)

-- | The will carry the state of an Entity, parametrized over Geo.Point type @p@
type EntityState p = State (Entity p)

-- ===========================================================================
--                       Exported Free Functions
-- ===========================================================================


-- | Returns an Entity that has nothing in it.
nullEntity :: Entity a
nullEntity = Entity Map.empty Map.empty Topo.emptyTopology

emptyEntityState :: EntityState a ()
emptyEntityState = pure ()

-- | Returns a list of all the "Topology.Vertex" in the "Entity"
getVertices :: Entity a -> [Topo.Vertex]
getVertices = Topo.getVertices . _getTopology

-- | Returns a list of all the "Topology.Edge" in the "Entity"
getEdges :: Entity a -> [Topo.Edge]
getEdges = Topo.getEdges . _getTopology

-- | Adds a "Vertex" to the "Entity".
--
--   A "Vertex" has both a "Geometry" (a "Point"), and a "Topology" (a
--   "Topology.Vertex")
addVertex :: Fractional a => Geo.Point a -> EntityState a Topo.Vertex
addVertex p = do
    -- First, retrieve the current state
    (Entity vmap emap t) <- get

    let -- Add a Vertex to the Topology
        (v, t')  = runState Topo.addFreeVertex t
        -- Pair the topological Vertex with the provided geometric Point
        vmap'    = Map.insert v p vmap

    -- Update our state with the new information
    put (Entity vmap' emap t')

    -- Give the user the generated "Topology.Vertex"
    pure v

-- | Adds an Edge to the Entity.
--
--   If either or both of the supplied "Topology.Vertex" are not currently
--   present in the Entity, Nothing is returned.
--
--   The created Edge will geometrically have a straight line between the two
--   Vertex
addEdge :: (Fractional a, Eq a) => Topo.Vertex -> Topo.Vertex -> EntityState a (Maybe Topo.Edge)
addEdge v1 v2 = runMaybeT $ do
    -- First, retrieve the current state
    (Entity vmap emap _) <- lift get

    -- Try to retrieve the points associated with these Vertices
    p1   <- MaybeT (getPoint' v1) :: MaybeT (EntityState a) (Geo.Point a)
    p2   <- MaybeT (getPoint' v2) :: MaybeT (EntityState a) (Geo.Point a)

    -- Bail out if the two points are geometrically equivalent
    when (p1 == p2) mzero

    -- Try to add the Edge to the topology
    (edge, t') <- MaybeT (addEdge' v1 v2)

    let -- We'll make a geometric straight line between the two points
        line = Geo.makeLine p1 p2
        -- Pair the topological Edge with the line we made earlier
        emap' = Map.insert edge line emap

    -- Update our state with the new information
    lift (put (Entity vmap emap' t'))

    -- Give the user a reference to the new Edge
    pure edge

-- | Returns the underlying geometric Point of the Vertex
--   
--   Returns Nothing if the Vertex is not part of this Entity
getPoint :: Entity a -> Topo.Vertex -> Maybe (Geo.Point a)
getPoint entity vertex = evalState (getPoint' vertex) entity

-- | Returns the Point associated with the given Vertex
--
--   Return Nothing if the Vertex is not part of this Entity
getPoint' :: Topo.Vertex -> EntityState a (Maybe (Geo.Point a))
getPoint' vertex = gets getVertexMap >>= pure . (Map.lookup vertex)

addEdge' :: Topo.Vertex -> Topo.Vertex -> EntityState a (Maybe (Topo.Edge, Topo.Topology))
addEdge' v1 v2 = runMaybeT $ do
    -- Unwrap the Topology from the EntityState
    topology <- lift (gets _getTopology)

    -- Try to add the given Edge
    let (maybeEdge, t') = runState (Topo.addEdge v1 v2) topology
    edge <- MaybeT . pure $ maybeEdge

    -- Return the added Edge and the updated Topology
    pure (edge, t')

-- | Returns any "Vertex" that have the given "Geometry"
getVertex :: Eq a => Entity a -> Geo.Point a -> [Topo.Vertex]
getVertex entity point = Map.keys $ Map.filter ((==) point) (getVertexMap entity)

-- | Returns the underlying geometric "Curve" of the "Edge'"
getCurve :: Entity a -> Topo.Edge -> Maybe (Geo.Line a)
getCurve e = (`Map.lookup` (getEdgeMap e))

-- | Returns the opposite "Vertex".
--
--   In this context, \"opposite\" means that it is on the other side of the
--   "Edge".
--
--   Any given "Edge" can only ever have two "Vertex" attached to it (this is
--   enforced by the "Topology" module), and as such there is only ever one
--   \"opposite\" "Vertex"
oppositeVertex :: Eq a => Entity a -> Topo.Vertex -> Topo.Edge -> Maybe Topo.Vertex
oppositeVertex = undefined
--oppositeVertex e@(Entity _ _ t) (Vertex _ v1) (Edge _ ed) = v2
    --where xs = Topo.adjVertToEdge t ed
          --v2 | (length xs) /= 2 = Nothing
             -- | a == v1        = getVertex' e b
             -- | b == v1        = getVertex' e a
             -- | otherwise      = Nothing
             --where a = xs !! 0
                   --b = xs !! 1

prettyPrintEntity :: Show a => Entity a -> Doc ann
prettyPrintEntity _ = undefined
--prettyPrintEntity entity@(Entity vs es _) = undefined
    --case show doc of
        --"" -> pretty "null entity"
        --_  -> doc
    --where vs' = vsep $ reverse (map (prettyPrintVertex entity) vs)
          --es' = vsep $ reverse (map (prettyPrintEdge entity) es)
          --func d = if show d == "" then d else d <> line
          --doc = foldMap func [vs',es']

-- ===========================================================================
--                       Private Free Functions
-- ===========================================================================
