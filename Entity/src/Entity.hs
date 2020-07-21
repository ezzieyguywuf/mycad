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
  Edge
, Entity
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
, prettyPrintEdge
) where

-- Base
import Control.Monad.State (State, get, runState, put)

-- Third-party
import qualified Data.Map as Map
import Data.Text.Prettyprint.Doc (Doc, nest, vsep, pretty)

-- Internal
import qualified Geometry as Geo
import qualified Topology as Topo

-- ===========================================================================
--                               Data Types
-- ===========================================================================
-- | This maps a "Topology.Vertex" to a specfic "Geometry.Point".
type VertexMap p = Map.Map Topo.Vertex (Geo.Point p)

-- | Similar to "Vertex", this provides extra "Geometry" information that is
--   not present in "Topology.Edge"
--
--   Also like "Vertex", the parametrization specifies the precision of the
--   "Geo.Point"
data Edge   p = Edge {
                       getGeoLine  :: (Geo.Line p)
                     , getTopoEdge :: Topo.Edge
                     }
                     deriving (Show, Eq)

-- | The main data type, encapsulating an entire "Topology" and associated
--   "Geometry"
--
--   Please note that the "Vertex" and "Edge" stored here are distinctly *not*
--   "Topology.Vertex" nor "Topology.Edge". Rather, these are primitive
--   "Entity" data types that encapsulate both "Topology" and "Geometry"
data Entity p = Entity { getVertexMap :: VertexMap p
                       , getEdges :: [Edge p]
                       , _getTopology :: Topo.Topology
                       } deriving (Show)

-- | The will carry the state of an Entity, parametrized over Geo.Point type @p@
type EntityState p a = State (Entity p) a

-- ===========================================================================
--                       Exported Free Functions
-- ===========================================================================


-- | Returns an Entity that has nothing in it.
nullEntity :: Entity a
nullEntity = Entity Map.empty [] Topo.emptyTopology

emptyEntityState :: EntityState a ()
emptyEntityState = pure ()

-- | Returns a list of all the "Topology.Vertex" in the "Entity"
getVertices :: Entity a -> [Topo.Vertex]
getVertices = Topo.getVertices . _getTopology

-- | Adds a "Vertex" to the "Entity".
--
--   A "Vertex" has both a "Geometry" (a "Point"), and a "Topology" (a
--   "Topology.Vertex")
addVertex :: Fractional a => Geo.Point a -> EntityState a Topo.Vertex
addVertex p = do
    (Entity vmap es t) <- get
    let (v, t')  = runState Topo.addFreeVertex t
        vmap' = Map.insert v p vmap
    put $ Entity vmap' es t'
    pure v

-- | Adds an "Edge" to the "Entity".
--
--   An "Edge" has both a "Geometry" (a "Curve"), and a "Topology" (a
--   "Topology.Edge")
addEdge :: Fractional a => Geo.Point a -> Geo.Point a -> EntityState a (Edge a)
addEdge _ _ = undefined
    --(Entity vmap es t) <- get
    --let gline   = Geo.makeLine p1 p2
        --(Just e, t')  = runState (Topo.addRayEdge v1) t
        --(Just v2, t'')   = runState (Topo.closeRayEdge e) t'
        --vs' = (Vertex p2 v2) : vs
        --es' = (Edge gline e) : es
    --put $ Entity vs' es' t''
    --pure $ Edge gline e

-- | Returns the underlying geometric "Point" of the "Vertex"
getPoint :: Topo.Vertex -> Geo.Point a
getPoint = undefined

-- | Tries to find an "Topology.Vertex" at the give "Point"
getVertex :: Eq a => Entity a -> Geo.Point a -> Maybe Topo.Vertex
getVertex _ _ = undefined

-- | Returns the underlying geometric "Curve" of the "Edge'"
getCurve :: Edge a -> Geo.Line a
getCurve = getGeoLine

-- | Returns the opposite "Vertex".
--
--   In this context, \"opposite\" means that it is on the other side of the
--   "Edge".
--
--   Any given "Edge" can only ever have two "Vertex" attached to it (this is
--   enforced by the "Topology" module), and such there is only ever one
--   \"opposite\" "Vertex"
oppositeVertex :: Eq a => Entity a -> Topo.Vertex -> Edge a -> Maybe Topo.Vertex
oppositeVertex = undefined
--oppositeVertex e@(Entity _ _ t) (Vertex _ v1) (Edge _ ed) = v2
    --where xs = Topo.adjVertToEdge t ed
          --v2 | (length xs) /= 2 = Nothing
             -- | a == v1        = getVertex' e b
             -- | b == v1        = getVertex' e a
             -- | otherwise      = Nothing
             --where a = xs !! 0
                   --b = xs !! 1

prettyPrintEdge :: Show a => Entity a -> Edge a -> Doc ann
prettyPrintEdge (Entity _ _ t) (Edge _ e) =
    nest 4 $ vsep [e', pretty "Line"]
    where e' = Topo.prettyPrintEdge t e

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
