module VertexData
(
  line
, lineIndices
, lineData
, cube
, cubeIndices
, cubeData
, ModelData (..)
, makeMatrix
) where

-- gl, all types and funcs here will already start with "gl"
import Graphics.GL.Types
import Linear.V3
import Linear.V4
import Linear.Quaternion
import Linear.Matrix
import GraphicData

data ModelData = ModelData { getRotation :: Quaternion Float
                           , getLocation :: V3 Float
                           }
                           deriving (Show)

makeMatrix :: ModelData -> M44 Float
makeMatrix (ModelData rot trans)= mkTransformation rot trans

lineData :: [ModelData]
lineData =
    [
      ModelData (axisAngle (V3 0 0 0) 0) (V3 0 0 0)
    , ModelData (axisAngle (V3 0 0 0) 0) (V3 15 0 0)
    ]

line :: GraphicData
line =
    [
        [Position (V3 0 3 0), Color (V4 1.0 0.5 0.2 1)]
      , [Position (V3 0 0 0), Color (V4 1.0 0.5 0.2 1)]
      , [Position (V3 15 3 0), Color (V4 1.0 0.5 0.2 1)]
      , [Position (V3 15 3 0), Color (V4 0.0 0.5 0.2 1)]
      , [Position (V3 0 0 0), Color (V4 0.0 0.5 0.2 1)]
      , [Position (V3 15 0 0), Color (V4 0.0 0.5 0.2 1)]
    ]

lineIndices :: [GLuint]
lineIndices =
    [
      0, 1, 2
    , 3, 4, 5
    ]

cubeData :: [ModelData]
cubeData =
    [
      ModelData (axisAngle (V3 0 0 0) 0) (V3 60 10 0)
    ]

cube :: GraphicData
cube =
    [ 
      [Position (V3 (-10) (-10) 10), Color (V4 0.2 0.5 0.2 1)]    -- 0 Top, Bottom Left
    , [Position (V3    10 (-10) 10), Color (V4 0.2 0.5 0.2 1) ]    -- 1 Top, Bottom Right
    , [Position (V3    10    10 10), Color (V4 1.0 0.5 0.2 1) ]    -- 2 Top, Top Right
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
