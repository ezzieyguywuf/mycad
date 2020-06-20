module VertexData
(
  line
, line'
, line''
, cube
, circle
, makeLine
, makeLine'
, makeLine''
, extendLine
, wireCube
) where

-- gl, all types and funcs here will already start with "gl"
import Linear.V3
import Linear.V4
import Linear.Quaternion
import Linear.Metric
import GraphicData

lineElement' :: Float -> V4 Float-> ElementData
lineElement' len color = makeElementData vertices indices
    where vertices = [ [Position (V3 0 0 0 ), Direction (V3 1 0 0), Color color, Up 1]
                     , [Position (V3 0 0 0 ), Direction (V3 1 0 0), Color color, Up (-1)]
                     , [Position (V3 len 0 0 ), Direction (V3 1 0 0), Color color, Up (1)]
                     , [Position (V3 0 0 0 ), Direction (V3 1 0 0), Color color, Up 1]
                     , [Position (V3 len 0 0 ), Direction (V3 1 0 0), Color color, Up (1)]
                     , [Position (V3 len 0 0 ), Direction (V3 1 0 0), Color color, Up (-1)]
                     ]
          indices = [0, 1, 2, 3, 4, 5]

lineElement'' :: Float -> V4 Float -> V4 Float -> ElementData
lineElement'' len color1 color2 = makeElementData vertices indices
    where vertices = [ [Position (V3 0 0 0 ), Direction (V3 1 0 0), Color color1, Up 1]
                     , [Position (V3 0 0 0 ), Direction (V3 1 0 0), Color color1, Up (-1)]
                     , [Position (V3 len 0 0 ), Direction (V3 1 0 0), Color color1, Up (1)]
                     , [Position (V3 0 0 0 ), Direction (V3 1 0 0), Color color2, Up 1]
                     , [Position (V3 len 0 0 ), Direction (V3 1 0 0), Color color2, Up (1)]
                     , [Position (V3 len 0 0 ), Direction (V3 1 0 0), Color color2, Up (-1)]
                     ]
          indices = [0, 1, 2, 3, 4, 5]

lineElement :: Float -> Float -> ElementData
lineElement width length = makeElementData vertices indices
    where vertices = [ [Position (V3 0 width 0 )     , Color (V4 1.0 0.5 0.2 1)]
                     , [Position (V3 0 0 0)          , Color (V4 1.0 0.5 0.2 1)]
                     , [Position (V3 length width 0) , Color (V4 1.0 0.5 0.2 1)]
                     , [Position (V3 length width 0) , Color (V4 0.0 0.5 0.2 1)]
                     , [Position (V3 0 0 0)          , Color (V4 0.0 0.5 0.2 1)]
                     , [Position (V3 length 0 0)     , Color (V4 0.0 0.5 0.2 1)]
                     ]
          indices = [ 0, 1, 2
                    , 3, 4, 5 ]

makeLine :: Float         -- ^ Width
            -> V3 Float   -- ^ from
            -> V3 Float   -- ^ to
            -> ObjectData -- ^ drawable object
makeLine width p1 p2 = ObjectData eData [placeLine p1 p2]
    where eData  = lineElement width length
          length = norm (p2 - p1)

makeLine' ::   V3 Float   -- ^ from
            -> V3 Float   -- ^ to
            -> V4 Float   -- ^ color
            -> ObjectData -- ^ drawable object
makeLine' p1 p2 color = ObjectData eData [placeLine p1 p2]
    where eData  = lineElement' length color
          length = norm (p2 - p1)

makeLine'' ::   V3 Float   -- ^ from
            -> V3 Float   -- ^ to
            -> V4 Float   -- ^ color1
            -> V4 Float   -- ^ color2
            -> ObjectData -- ^ drawable object
makeLine'' p1 p2 color1 color2 = ObjectData eData [placeLine p1 p2]
    where eData  = lineElement'' length color1 color2
          length = norm (p2 - p1)

placeLine :: V3 Float -> V3 Float -> PlacementData
placeLine p1 p2 = PlacementData (axisAngle axis theta) p1
    where vect = p2 - p1
          basis = V3 1 0 0 -- Because LineElement is in the positive x-direction
          axis  = (V3 1 0 0) `cross` vect
          theta = acos (((normalize vect) `dot` basis))

extendLine :: ObjectData -> V3 Float -> V3 Float -> ObjectData
extendLine (ObjectData eData placementData) p1 p2 = ObjectData eData ((placeLine p1 p2) : placementData)

circle :: ObjectData
circle = foldl (\ object (p1, p2) -> extendLine object p1 p2) obj pairs
    where radius = 30
          center = V3 (-15) (-15) 0
          thetas = [0, pi/20..2*pi]
          points = map (pointOnCircle center radius) thetas
          pairs  = zip points (tail points)
          obj = makeLine' (points !! 0) (points !! 1) (V4 0 0 0 0)

pointOnCircle :: V3 Float -> Float -> Float -> V3 Float
pointOnCircle (V3 c1 c2 c3) radius theta = V3 x y z
    where x = c1 + radius * cos theta
          y = c2 + radius * sin theta
          z = c3

line :: ObjectData
line = extendLine line0 (V3 15 0 0) (V3 30 (-15) 0)
    where line0 = makeLine 3 (V3 0 0 0) (V3 15 0 0)

wireCube :: ObjectData
wireCube = foldl (\l (p,q) -> extendLine l p q) line0 pairs
    where line0 = makeLine 3 (ps !! 0) (ps !! 1)
          ps = [ V3 (-10) 10 10
               , V3 (-20) 10 10
               , V3 (-20) 20 10
               , V3 (-10) 20 10
               , V3 (-10) 10 10
               , V3 (-10) 10 20
               , V3 (-20) 10 20
               , V3 (-20) 20 20
               , V3 (-10) 20 20
               , V3 (-10) 10 20
               ]
          pairs = zip ps (tail ps)

line' :: ObjectData
line' = ObjectData eData placementData
    where (ObjectData eData pOrig) = line
          f (PlacementData rot trans) = PlacementData rot (trans + (V3 0 30 0))
          placementData = map f pOrig

line'' :: ObjectData
line'' = ObjectData eData placementData
    where (ObjectData eData pOrig) = line
          f (PlacementData rot trans) = PlacementData rot (trans + (V3 (-50) 00 0))
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
