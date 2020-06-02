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
, getComponents
  -- * Inspection
, getVertices
, getEdges
) where

import qualified Geometry as Geo
import qualified Topology as Topo

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
                       }

-- ===========================================================================
--                       Exported Free Functions
-- ===========================================================================


nullEntity :: Entity a
nullEntity = Entity [] [] Topo.emptyTopology

addVertex :: Fractional a => Entity a -> a -> a -> a -> Entity a
addVertex (Entity vs es t) x y z = Entity vs' es t'
    where p   = Geo.makePoint x y z
          t'  = Topo.addVertex $ t
          v   = last $ Topo.getVertices t'
          vs' = (Glue p v) : vs

getComponents :: Vertex a -> (a, a, a)
getComponents v = (x, y, z)
    where p = getGeo v
          [x, y, z] = Geo.getComponents p
