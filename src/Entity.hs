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
  -- * Creation
, nullEntity
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

type Vertex = Glue (Geo.Point Int) Topo.Vertex
type Edge   = Glue (Geo.Line Int) Topo.Edge

data Entity = Entity { getVertices :: [Vertex]
                     , getEdges :: [Edge]
                     }

-- ===========================================================================
--                       Exported Free Functions
-- ===========================================================================


nullEntity :: Entity
nullEntity = Entity [] []
