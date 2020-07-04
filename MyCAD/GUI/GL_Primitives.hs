module GL_Primitives
(
  makeLine
)where

-- Third-party
import Linear.V1 (V1(V1))
import Linear.V3 (V3)
import Linear.V4 (V4)

-- internal
import GraphicData (ElementData, VertexAttribute(..), makeElementData)

-- | You can specify a color for each triangle.
--
--   This allows you to draw a line and see the triangles that make it up. You
--   may be able to use this creatively to make a line with a gradient
makeLine ::   V4 Float    -- ^ color1
           -> V4 Float    -- ^ color2
           -> V3 Float    -- ^ from
           -> V3 Float    -- ^ to
           -> ElementData -- ^ drawable element
makeLine c1 c2 p1 p2 = makeElementData vertices indices
    where vertices = [ [Position p1, Direction p2, Color c1, Up (V1   1) ]
                     , [Position p1, Direction p2, Color c1, Up (V1 (-1))]
                     , [Position p2, Direction p2, Color c1, Up (V1   1) ]
                     , [Position p1, Direction p2, Color c2, Up (V1 (-1))]
                     , [Position p2, Direction p2, Color c2, Up (V1   1) ]
                     , [Position p2, Direction p2, Color c2, Up (V1 (-1))]
                     ]
          indices = [0, 1, 2, 3, 4, 5]
