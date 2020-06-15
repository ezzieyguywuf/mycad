module VertexData
(
  line
, lineIndices
, lineLocations
, cube
, cubeIndices
, cubeLocations
) where

-- gl, all types and funcs here will already start with "gl"
import Graphics.GL.Types
import Linear.V2
import Linear.V3
import Linear.V4
import GraphicData

lineLocations :: [V3 Float]
lineLocations =
    [
      V3 0 0 0
    , V3 4 6 0
    ]
line :: GraphicData
line =
    [
        [Position (V3 0 0 0), Texture (V2 0 0)]
      , [Position (V3 3 0 0), Texture (V2 0 0)]
      , [Position (V3 3 5 0), Texture (V2 0 0)]
    ]

lineIndices :: [GLuint]
lineIndices =
    [
      0, 1, 2
    ]

cubeLocations :: [V3 Float]
cubeLocations =
    [
      V3 10 10 0
    ]

cube :: GraphicData
cube =
    [ 
      [Position (V3 (-10) (-10) 10), Color (V4 0.2 0.5 0.2 1)]    -- 0 Top, Bottom Left
    , [Position (V3    10 (-10) 10), Color (V4 0.2 0.5 0.2 1) ]    -- 1 Top, Bottom Right
    , [Position (V3    10    10 10), Color (V4 0.2 0.5 0.2 1) ]    -- 2 Top, Top Right
    , [Position (V3 (-10)    10 10), Color (V4 0.2 0.5 0.2 1) ]    -- 3 Top, Top Left

    , [Position (V3 (-10) (-10) (-10)), Color (V4 0.2 0.5 0.2 1) ]     -- 4 Bottom, Bottom Left
    , [Position (V3    10 (-10) (-10)), Color (V4 0.2 0.5 0.2 1) ]   -- 5 Bottom, Bottom Right
    , [Position (V3    10    10 (-10)), Color (V4 0.2 0.5 0.2 1) ] -- 6 Bottom, Top Left
    , [Position (V3 (-10)    10 (-10)), Color (V4 0.2 0.5 0.2 1) ] -- 7 Bottom, Top Right

    ]

cubeIndices :: [GLuint]
cubeIndices = [ 
                0, 1, 2 -- Top
              , 0, 2, 3
              , 0, 1, 4 -- Front
              , 1, 4, 5
              , 2, 3, 6 -- Back
              , 3, 6, 7
              , 4, 5, 6 -- 'Front
              , 4, 6, 7
              , 0, 4, 7 -- Left
              , 0, 3, 7
              , 1, 2, 5 -- Right
              , 5, 2, 6
              ]
