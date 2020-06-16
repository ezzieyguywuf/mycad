module VertexData
(
  line
, line'
, cube
, cubeElements
, ElementData (..)
, GeoData (..)
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

data ElementData = ElementData { elementRotation :: Quaternion Float
                               , elementTranslation :: V3 Float
                               }
                               deriving (Show)

-- | Any given Element can be re-used as often as you like
data GeoData = GeoData { getIndices :: Element
                       , getGeoData :: [ElementData]
                       }
                       deriving (Show)

-- | Creates a transformation matrix given the ElementData
makeMatrix :: ElementData -> M44 Float
makeMatrix (ElementData rot trans)= mkTransformation rot trans

line :: ObjectData
line = ObjectData eData placementData
    where eData = makeElementData' line lineIndices
          line =
              [
                  [Position (V3 0 3 0), Color (V4 1.0 0.5 0.2 1)]
                , [Position (V3 0 0 0), Color (V4 1.0 0.5 0.2 1)]
                , [Position (V3 15 3 0), Color (V4 1.0 0.5 0.2 1)]
                , [Position (V3 15 3 0), Color (V4 0.0 0.5 0.2 1)]
                , [Position (V3 0 0 0), Color (V4 0.0 0.5 0.2 1)]
                , [Position (V3 15 0 0), Color (V4 0.0 0.5 0.2 1)]
              ]
          lineIndices =
              [
                0, 1, 2
              , 3, 4, 5
              ]
          placementData =
              [
                PlacementData { placementRotation=(axisAngle (V3 0 0 0) 0)
                              , placementTranslation=(V3 0 0 0)}
              , PlacementData { placementRotation=(axisAngle (V3 0 0 1) (-pi/4))
                              , placementTranslation=(V3 15 0 0)}
              ]
line' :: ObjectData
line' = ObjectData eData placementData
    where (ObjectData eData pOrig) = line
          f (PlacementData rot trans) = PlacementData rot (trans + (V3 0 30 0))
          placementData = map f pOrig

cubeElements :: GeoData
cubeElements = GeoData cubeIndices cubeData

cubeData :: [ElementData]
cubeData =
    [
      ElementData (axisAngle (V3 0 0 0) 0) (V3 60 10 0)
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
