module VertexData
(
  line
, line'
, cube
, circle
, makeLine
) where

-- gl, all types and funcs here will already start with "gl"
import Linear.V3
import Linear.V4
import Linear.Quaternion
import Linear.Metric
import Linear.Vector
import GraphicData
import ViewSpace

circle = undefined

makeLine :: Camera        -- ^ View direction for the line
            -> V3 Float   -- ^ from
            -> V3 Float   -- ^ to
            -> Float      -- ^ Line width
            -> ObjectData -- ^ drawable object
makeLine (LookAt loc up dir) p1 p2 width = ObjectData eData placementData
    where eData    = makeElementData verts indices
          perp = normalize $ (loc - dir) `cross` (p2 - p1)
          delta = (width / 2) *^ perp
          v1    = p1 + delta
          v2    = p1 - delta
          v3    = p2 + delta
          v4    = p2 - delta
          verts = [ [Position v1, Color (V4 1.0 0.5 0.2 1)]
                  , [Position v2, Color (V4 1.0 0.5 0.2 1)]
                  , [Position v3, Color (V4 1.0 0.5 0.2 1)]
                  , [Position v3, Color (V4 0.0 0.5 0.2 1)]
                  , [Position v2, Color (V4 0.0 0.5 0.2 1)]
                  , [Position v4, Color (V4 0.0 0.5 0.2 1)]
                  ]
          indices =
              [
                0, 1, 2
              , 3, 4, 5
              ]
          placementData =
              [
                PlacementData { placementRotation=(axisAngle (V3 0 0 0) 0)
                              , placementTranslation=(V3 0 0 0)}
              ]

--nextLine :: ObjectData -> V3 Float -> ObjectData
--nextLine 

line :: ObjectData
line = ObjectData eData placementData
    where eData = makeElementData line lineIndices
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

cube :: ObjectData
cube = ObjectData eData placementData
    where eData = makeElementData cube cubeIndices
          cube  =
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
          placementData =
                [
                  PlacementData { placementRotation=(axisAngle (V3 0 0 0) 0)
                                , placementTranslation=(V3 60 10 0)}
                ]
