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
import Control.Monad.State

-- ===========================================================================
--                               Data Types
-- ===========================================================================
data Vertex a = Vertex {
                         getGeoPoint   :: Geo.Point a
                       , getTopoVertex :: Topo.Vertex
                       }
                       deriving (Show, Eq)

data Edge   a = Edge {
                       getGeoLine  :: (Geo.Line a)
                     , getTopoEdge :: Topo.Edge
                     }
                     deriving (Show, Eq)

data Entity a = Entity { getVertices :: [Vertex a]
                       , getEdges :: [Edge a]
                       , _getTopology :: Topo.Topology
                       } deriving (Show)

type EntityState b a = State (Entity b) a

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
