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
      V3 0 0 0
    , V3 20 0 0
    , V3 40 0 0
    , V3 60 0 0
    , V3 80 0 0
    , V3 0 20 0
    , V3 0 40 0
    , V3 0 60 0
    , V3 0 80 0
    , V3 20 20 0
    , V3 40 40 0
    , V3 60 60 0
    , V3 80 80 0
    ]

cube :: GraphicData
cube =
    [ 
      [Position (V3 (-10) (-10) (-10)) , Texture (V2 0 0)]      -- 0 Back, Bottom Left
    , [Position (V3    10 (-10) (-10)) , Texture (V2 1.0 0)]    -- 1 Back, Bottom Right
    , [Position (V3    10    10 (-10)) , Texture (V2 1.0 1.0)]  -- 2 Back, Top Left
    , [Position (V3 (-10)    10 (-10)) , Texture (V2 0.0 1.0)]  -- 3 Back, Top Right

    , [Position (V3 (-10) (-10) 10) , Texture (V2 1.0 1.0)] -- 4 Front, Bottom Left
    , [Position (V3    10 (-10) 10) , Texture (V2 0 1.0)]   -- 5 Front, Bottom Right
    , [Position (V3    10    10 10) , Texture (V2 0 0)]     -- 6 Front, Top Left
    , [Position (V3 (-10)    10 10) , Texture (V2 1.0 0)]   -- 7 Front, Top Right
    ]

cubeIndices :: [GLuint]
cubeIndices = [ 0, 1, 2 -- Back
              , 0, 2, 3
              , 0, 4, 7 -- Left
              , 0, 3, 7
              , 4, 5, 6 -- Front
              , 4, 6, 7
              , 1, 2, 5 -- Right
              , 5, 2, 6
              , 2, 3, 6 -- Top
              , 3, 6, 7
              , 0, 1, 4 -- Bottom
              , 1, 4, 5
              ]
