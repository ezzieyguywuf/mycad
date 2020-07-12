module GUI.GL.Primitives
(
  makeLine
)where

-- Third-party
import Linear.V1 (V1(V1))
import Linear.V3 (V3(V3))
import Linear.V4 (V4(V4))
import Linear.Quaternion (axisAngle)

-- internal
import GUI.GraphicData (ObjectData, VertexAttribute(..)
                       , makeElementData, placeElement, makePlacement)

-- | Create the "ObjectData" needed to render a line through the two points
makeLine ::   V3 Float    -- ^ from
           -> V3 Float    -- ^ to
           -> ObjectData  -- ^ drawable element
makeLine p1 p2 = placeElement eData placement
    where  eData    = makeElementData vertices indices
           vertices = [ [Position p1, Direction p2, Color c1, Up (V1   1) ]
                     , [Position p1, Direction p2, Color c1, Up (V1 (-1))]
                     , [Position p2, Direction p1, Color c1, Up (V1   (-1)) ]
                     , [Position p1, Direction p2, Color c2, Up (V1 (-1))]
                     , [Position p2, Direction p1, Color c2, Up (V1   1) ]
                     , [Position p2, Direction p1, Color c2, Up (V1 (-1))]
                     ]
           indices     = [0, 1, 2, 3, 4, 5]
           rotation    = axisAngle (V3 0 0 0) 0
           translation = V3 0 0 0
           placement   = makePlacement rotation translation
           c1          = V4 0.0 0.0 0.0 1.0
           c2          = V4 0.0 0.0 0.0 1.0
           --c1          = V4 0.2 0.0 0.2 1.0
           --c2          = V4 0.7 0.4 0.7 1.0
