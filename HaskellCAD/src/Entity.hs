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
data Glue g t = Glue { getGeo :: g
                     , getTopo :: t
                     }
                      deriving (Show, Eq)

type Vertex a = Glue (Geo.Point a) Topo.Vertex
type Edge   a = Glue (Geo.Line a) Topo.Edge

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

addVertex :: Fractional a => Geo.Point a -> EntityState a (Vertex a)
addVertex p = do
    (Entity vs es t) <- get
    let (v, t')  = runState Topo.addFreeVertex t
        newVertex = Glue p v
        vs' = newVertex : vs
    put $ Entity vs' es t'
    pure newVertex

addEdge :: Fractional a => Vertex a -> Geo.Point a -> EntityState a (Edge a)
addEdge (Glue p1 v1) p2 = do
    (Entity vs es t) <- get
    let line   = Geo.makeLine p1 p2
        (Just e, t')  = runState (Topo.addRayEdge v1) t
        (Just v2, t'')   = runState (Topo.closeRayEdge e) t'
        vs' = (Glue p2 v2) : vs
        es' = (Glue line e) : es
    put $ Entity vs' es' t''
    pure $ Glue line e

getPoint :: Entity a -> Vertex a -> Maybe (Geo.Point a)
getPoint e (Glue _ v) = do
    p <- find f (getVertices e)
    return $ getGeo p
    where f x = (getTopo x) == v

getVertex :: Eq a => Entity a -> Geo.Point a -> Maybe (Vertex a)
getVertex entity point = find pred (getVertices entity)
    where pred x = (getGeo x) == point

getCurve :: Entity a -> Edge a -> Maybe (Geo.Line a)
getCurve ent (Glue c e) = do
    find f (getEdges ent) >>= \_ -> return c
    where f x = (getTopo x) == e

oppositeVertex :: Eq a => Entity a -> Vertex a -> Edge a -> Maybe (Vertex a)
oppositeVertex e@(Entity _ _ t) (Glue _ v1) (Glue _ ed) = v2
    where xs = Topo.adjVertToEdge t ed
          v2 | (length xs) /= 2 = Nothing
             | a == v1        = getVertex' e b
             | b == v1        = getVertex' e a
             | otherwise      = Nothing
             where a = xs !! 0
                   b = xs !! 1

prettyPrintVertex :: Show a => Entity a -> Vertex a -> Doc ann
prettyPrintVertex (Entity _ _ t) (Glue p v) = nest 4 $ vsep [v', p']
    where p'  = pretty $ show p
          v'  = Topo.prettyPrintVertex t v

prettyPrintVertices :: Show a => Entity a -> Doc ann
prettyPrintVertices e = vsep $ reverse vs
    where vs = map (prettyPrintVertex e) $ getVertices e

prettyPrintEdge :: Show a => Entity a -> Edge a -> Doc ann
prettyPrintEdge (Entity _ _ t) (Glue _ e) =
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
    where f x = (getTopo x) == v
