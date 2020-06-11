module VertexData
(
  vertices
, indices
, cube
, cubeIndices
) where

-- gl, all types and funcs here will already start with "gl"
import Graphics.GL.Types
import Linear.V2
import Linear.V3
import GraphicData

cube :: GraphicData
cube =
    [ 
      [Position (V3 (-0.5) (-0.5) 0) , Texture (V2 0 0)]      -- 0 Back, Bottom Left
    , [Position (V3    0.5 (-0.5) 0) , Texture (V2 1.0 0)]    -- 1 Back, Bottom Right
    , [Position (V3    0.5    0.5 0) , Texture (V2 1.0 1.0)]  -- 2 Back, Top Left
    , [Position (V3 (-0.5)    0.5 0) , Texture (V2 0.0 1.0)]  -- 3 Back, Top Right

    , [Position (V3 (-0.5) (-0.5) 0.5) , Texture (V2 0 0)]    -- 4 Front, Bottom Left
    , [Position (V3    0.5 (-0.5) 0.5) , Texture (V2 1.0 0)]  -- 5 Front, Bottom Right
    , [Position (V3    0.5    0.5 0.5) , Texture (V2 1.0 1.0)]-- 6 Front, Top Left
    , [Position (V3 (-0.5)    0.5 0.5) , Texture (V2 0.0 1.0)]-- 7 Front, Top Right
    ]

cubeIndices :: [GLuint]
cubeIndices = [ 0, 1, 2 -- Back
              , 0, 2, 3
              ]

--          positions  Texture Coords
vertices :: [GLfloat]
vertices = [  -- A cube
             -0.5, -0.5, -0.5,  0.0, 0.0
           ,  0.5, -0.5, -0.5,  1.0, 0.0
           ,  0.5,  0.5, -0.5,  1.0, 1.0
           ,  0.5,  0.5, -0.5,  1.0, 1.0
           , -0.5,  0.5, -0.5,  0.0, 1.0
           , -0.5, -0.5, -0.5,  0.0, 0.0

           , -0.5, -0.5,  0.5,  0.0, 0.0
           ,  0.5, -0.5,  0.5,  1.0, 0.0
           ,  0.5,  0.5,  0.5,  1.0, 1.0
           ,  0.5,  0.5,  0.5,  1.0, 1.0
           , -0.5,  0.5,  0.5,  0.0, 1.0
           , -0.5, -0.5,  0.5,  0.0, 0.0

           , -0.5,  0.5,  0.5,  1.0, 0.0
           , -0.5,  0.5, -0.5,  1.0, 1.0
           , -0.5, -0.5, -0.5,  0.0, 1.0
           , -0.5, -0.5, -0.5,  0.0, 1.0
           , -0.5, -0.5,  0.5,  0.0, 0.0
           , -0.5,  0.5,  0.5,  1.0, 0.0

           ,  0.5,  0.5,  0.5,  1.0, 0.0
           ,  0.5,  0.5, -0.5,  1.0, 1.0
           ,  0.5, -0.5, -0.5,  0.0, 1.0
           ,  0.5, -0.5, -0.5,  0.0, 1.0
           ,  0.5, -0.5,  0.5,  0.0, 0.0
           ,  0.5,  0.5,  0.5,  1.0, 0.0

           , -0.5, -0.5, -0.5,  0.0, 1.0
           ,  0.5, -0.5, -0.5,  1.0, 1.0
           ,  0.5, -0.5,  0.5,  1.0, 0.0
           ,  0.5, -0.5,  0.5,  1.0, 0.0
           , -0.5, -0.5,  0.5,  0.0, 0.0
           , -0.5, -0.5, -0.5,  0.0, 1.0

           , -0.5,  0.5, -0.5,  0.0, 1.0
           ,  0.5,  0.5, -0.5,  1.0, 1.0
           ,  0.5,  0.5,  0.5,  1.0, 0.0
           ,  0.5,  0.5,  0.5,  1.0, 0.0
           , -0.5,  0.5,  0.5,  0.0, 0.0
           , -0.5,  0.5, -0.5,  0.0, 1.0
           ]

indices :: [GLuint]
indices  = [ 0, 1, 2
           , 2, 3, 0]

