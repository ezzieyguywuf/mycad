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

-- ===========================================================================
--                       Exported Free Functions
-- ===========================================================================


nullEntity :: Entity a
nullEntity = Entity [] [] Topo.emptyTopology

addVertex :: Fractional a => Entity a -> Geo.Point a -> Entity a
addVertex (Entity vs es t) p = Entity vs' es t'
    where t'  = Topo.addVertex $ t
          v   = last . Topo.getVertices $ t'
          vs' = (Glue p v) : vs

addEdge :: Fractional a => Entity a -> Vertex a -> Geo.Point a -> Entity a
addEdge (Entity vs es t) v p2 = Entity vs' es' t''
    where t'  = Topo.addVertex t
          _v1  = getTopo v
          v2  = last $ Topo.getVertices t'
          p1  = getGeo v
          c   = Geo.makeLine p1 p2
          t'' = undefined
          e   = last $ Topo.getEdges t''
          vs' = (Glue p2 v2) : vs
          es' = (Glue c e) : es

getPoint :: Entity a -> Vertex a -> Maybe (Geo.Point a)
getPoint e (Glue _ v) = do
    p <- find f (getVertices e)
    return $ getGeo p
    where f x = (getTopo x) == v

getVertex :: Eq a => Entity a -> Geo.Point a -> Maybe (Vertex a)
getVertex e p = find f (getVertices e)
    where f x = (getGeo x) == p

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
    where p'  = pretty $ map show (Geo.getComponents p)
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
