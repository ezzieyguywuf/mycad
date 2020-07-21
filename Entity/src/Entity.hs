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

-- Base
import Data.Maybe (fromJust)
import Control.Monad.State (State, get, runState, put)

-- Third-party
import qualified Data.Map as Map
import Data.Text.Prettyprint.Doc (Doc)

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
                       } deriving (Show)

-- | The will carry the state of an Entity, parametrized over Geo.Point type @p@
type EntityState p a = State (Entity p) a

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
    (Entity vmap emap t) <- get
    let (v, t')  = runState Topo.addFreeVertex t
        vmap' = Map.insert v p vmap
    put $ Entity vmap' emap t'
    pure v

-- | Adds an "Edge" to the "Entity".
--
--   An "Edge" has both a "Geometry" (a "Curve"), and a "Topology" (a
--   "Topology.Edge")
addEdge :: Fractional a => Geo.Point a -> Geo.Point a -> EntityState a Topo.Edge
addEdge p1 p2 = do
    (Entity vmap emap t) <- get
    let ((v1,v2,edge), t') = runState topoState t
        topoState = do
            -- First, add a "free" vertex
            v1' <- Topo.addFreeVertex
            -- Next, extend a "ray" edge from that vertex
            edge' <- Topo.addRayEdge v1
            -- finally, close the "ray" edge
            v2' <- Topo.closeRayEdge (fromJust edge)
            pure (v1', v2', edge')
        -- Update the VertexMap to include the new vertices
        vmap' = Map.insert v1 p1 (Map.insert (fromJust v2) p2 vmap)
        -- We need to construct the geometric "Line" that goes along with the
        -- "Edge" that was created earlier
        gline   = Geo.makeLine p1 p2
        -- Update the edge list
        emap' = Map.insert (fromJust edge) gline emap
    put $ Entity vmap' emap' t'
    pure $ (fromJust edge)

-- | Returns the underlying geometric "Point" of the "Vertex"
getPoint :: Entity a -> Topo.Vertex -> Maybe (Geo.Point a)
getPoint e v = Map.lookup v (getVertexMap e)

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
--   enforced by the "Topology" module), and such there is only ever one
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
