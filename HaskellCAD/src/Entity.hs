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
  Vertex
, Edge
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
, prettyPrintVertex
, prettyPrintVertices
, prettyPrintEdge
, prettyPrintEdges
) where

import qualified Geometry as Geo
import qualified Topology as Topo
import Data.Text.Prettyprint.Doc
import Data.List (find)
import Control.Monad.State (State, get, runState, put)

-- ===========================================================================
--                               Data Types
-- ===========================================================================
-- | This looks similar to "Topolog.Vertex" but is distinct, as it includes
--   "Geometry" information. This data type is parametrized over the type of
--   "Geo.Point".
--
--   This allows for easily changing the precision,  i.e. "Float" versus
--   "Rational"
data Vertex p = Vertex {
                         getGeoPoint   :: Geo.Point p
                       , getTopoVertex :: Topo.Vertex
                       }
                       deriving (Show, Eq)

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
data Entity p = Entity { getVertices :: [Vertex p]
                       , getEdges :: [Edge p]
                       , _getTopology :: Topo.Topology
                       } deriving (Show)

-- | The will carry the state of an Entity, parametrized over Geo.Point type @p@
type EntityState p = State (Entity p)

-- ===========================================================================
--                       Exported Free Functions
-- ===========================================================================


nullEntity :: Entity a
nullEntity = Entity [] [] Topo.emptyTopology

emptyEntityState :: EntityState a ()
emptyEntityState = pure ()

addVertex :: Fractional a => Geo.Point a -> EntityState a (Vertex a)
addVertex p = do
    (Entity vs es t) <- get
    let (v, t')  = runState Topo.addFreeVertex t
        newVertex = Vertex p v
        vs' = newVertex : vs
    put $ Entity vs' es t'
    pure newVertex

addEdge :: Fractional a => Vertex a -> Geo.Point a -> EntityState a (Edge a)
addEdge (Vertex p1 v1) p2 = do
    (Entity vs es t) <- get
    let line   = Geo.makeLine p1 p2
        (Just e, t')  = runState (Topo.addRayEdge v1) t
        (Just v2, t'')   = runState (Topo.closeRayEdge e) t'
        vs' = (Vertex p2 v2) : vs
        es' = (Edge line e) : es
    put $ Entity vs' es' t''
    pure $ Edge line e

getPoint :: Entity a -> Vertex a -> Maybe (Geo.Point a)
getPoint entity vertex = do
    mPoint <- find pred (getVertices entity)
    pure $ getGeoPoint mPoint
    where pred v = (getTopoVertex v) == (getTopoVertex vertex)

getVertex :: Eq a => Entity a -> Geo.Point a -> Maybe (Vertex a)
getVertex entity point = find pred (getVertices entity)
    where pred v = (getGeoPoint v) == point

getCurve :: Entity a -> Edge a -> Maybe (Geo.Line a)
getCurve entity (Edge line edge) = do
    find pred (getEdges entity)
    pure line
    where pred e = (getTopoEdge e) == edge

oppositeVertex :: Eq a => Entity a -> Vertex a -> Edge a -> Maybe (Vertex a)
oppositeVertex e@(Entity _ _ t) (Vertex _ v1) (Edge _ ed) = v2
    where xs = Topo.adjVertToEdge t ed
          v2 | (length xs) /= 2 = Nothing
             | a == v1        = getVertex' e b
             | b == v1        = getVertex' e a
             | otherwise      = Nothing
             where a = xs !! 0
                   b = xs !! 1

prettyPrintVertex :: Show a => Entity a -> Vertex a -> Doc ann
prettyPrintVertex (Entity _ _ t) (Vertex p v) = nest 4 $ vsep [v', p']
    where p'  = pretty $ show p
          v'  = Topo.prettyPrintVertex t v

prettyPrintVertices :: Show a => Entity a -> Doc ann
prettyPrintVertices e = vsep $ reverse vs
    where vs = map (prettyPrintVertex e) $ getVertices e

prettyPrintEdge :: Show a => Entity a -> Edge a -> Doc ann
prettyPrintEdge (Entity _ _ t) (Edge _ e) =
    nest 4 $ vsep [e', pretty "Line"]
    where e' = Topo.prettyPrintEdge t e

prettyPrintEdges :: Show a => Entity a -> Doc ann
prettyPrintEdges e = vsep $ reverse es
    where es = map (prettyPrintEdge e) $ getEdges e

prettyPrintEntity :: Show a => Entity a -> Doc ann
prettyPrintEntity e = vs <> line <> es
    where vs = prettyPrintVertices e
          es = prettyPrintEdges e

-- ===========================================================================
--                       Private Free Functions
-- ===========================================================================
getVertex' :: Eq a => Entity a -> Topo.Vertex -> Maybe (Vertex a)
getVertex' (Entity vs _ _) v = find f vs
    where f x = (getTopoVertex x) == v
