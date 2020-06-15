module VertexData
(
  cube
, cubeIndices
, cubeLocations
) where

-- gl, all types and funcs here will already start with "gl"
import Graphics.GL.Types
import Linear.V2
import Linear.V3
import GraphicData

cubeLocations :: [V3 Float]
cubeLocations =
    [
      V3 10 10 0
    ]

cube :: GraphicData
cube =
    [ 
      [Position (V3 (-10) (-10) 10) , Texture (V2 0.0 0.0)]    -- 0 Top, Bottom Left
    , [Position (V3    10 (-10) 10) , Texture (V2 1.0 0.0)]    -- 1 Top, Bottom Right
    , [Position (V3    10    10 10) , Texture (V2 1.0 1.0)]    -- 2 Top, Top Right
    , [Position (V3 (-10)    10 10) , Texture (V2 0.0 1.0)]    -- 3 Top, Top Left

    , [Position (V3 (-10) (-10) (-10)) , Texture (V2 0 1.0)]     -- 4 Bottom, Bottom Left
    , [Position (V3    10 (-10) (-10)) , Texture (V2 1.0 1.0)]   -- 5 Bottom, Bottom Right
    , [Position (V3    10    10 (-10)) , Texture (V2 1.0 0.0)] -- 6 Bottom, Top Left
    , [Position (V3 (-10)    10 (-10)) , Texture (V2 0.0 0.0)] -- 7 Bottom, Top Right

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
