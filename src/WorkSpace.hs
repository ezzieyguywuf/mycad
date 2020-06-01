{-|
Module      : WorkSpace
Description : A CAD workspace
Copyright   : (c) Wolfgang E. Sanyer, 2020
License     : MPL2
Maintainer  : WolfgangESanyer@Gmail.com
Stability   : experimental
Portability : POSIX

This module provides a workspace in which various operations can be performed to adjust
the Geometry and Topology of a CAD model.
-}
module WorkSpace
( -- * Exported Types
  -- | These are wrappers around Geometry and Topology data types, and also form the
  --   "glue" that connects a given Topology to a piece of Geometry.
  Vertex
, Edge
  -- * Creation
, emptyWorkSpace
  -- * Inspection
, entityCount
) where

import qualified Geometry as Geo
import qualified Topology as Topo

-- ===========================================================================
--                               Data Types
-- ===========================================================================
data Entity g t = Entity {getGeo :: g,
                          getTopo :: t}
                          deriving (Show, Eq)

type Vertex = Entity (Geo.Point Int) Topo.Vertex
type Edge   = Entity (Geo.Line Int) Topo.Edge

data WorkSpace = WorkSpace { getVertices :: [Vertex]
                           , getEdges :: [Edge]
                           }

-- ===========================================================================
--                       Exported Free Functions
-- ===========================================================================


emptyWorkSpace :: WorkSpace
emptyWorkSpace = WorkSpace [] []

entityCount :: WorkSpace -> Int
entityCount ws = vs + es
    where vs = (length . getVertices) ws
          es = (length . getEdges) ws
