module VertexData
(
  line
, cube
, lineElements
, cubeElements
, ModelData (..)
, ElementData (..)
, makeMatrix
) where

-- gl, all types and funcs here will already start with "gl"
import Graphics.GL.Types
import Linear.V3
import Linear.V4
import Linear.Quaternion
import Linear.Matrix
import GraphicData

-- | An Element is a set of Vertices that can be reused in different locations/rotations
type Element= [GLuint]

data ModelData = ModelData { getRotation :: Quaternion Float
                           , getLocation :: V3 Float
                           }
                           deriving (Show)

-- | Any given Element can be re-used as often as you like
data ElementData = ElementData { getIndices :: Element
                               , getGeoData :: [ModelData]
                               }
                               deriving (Show)

-- | Creates a transformation matrix given the ModelData
makeMatrix :: ModelData -> M44 Float
makeMatrix (ModelData rot trans)= mkTransformation rot trans

lineElements :: ElementData
lineElements = ElementData lineIndices lineData

lineData :: [ModelData]
lineData =
    [
      ModelData (axisAngle (V3 0 0 0) 0) (V3 0 0 0)
    , ModelData (axisAngle (V3 0 0 1) (-pi/4)) (V3 15 0 0)
    ]

lineIndices :: Element
lineIndices =
    [
      0, 1, 2
    , 3, 4, 5
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

cubeElements :: ElementData
cubeElements = ElementData cubeIndices cubeData

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

cubeIndices :: Element
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
