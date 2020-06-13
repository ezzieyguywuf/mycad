module ViewSpace
( Camera(..)
, rotateCamera
)where

import Linear.V3
import Linear.Quaternion
import Linear.Metric
import Linear.Vector
import Data.IORef
import Numeric

data Camera = ArcBall { getRadius   :: Float
                      , getRotation :: Quaternion Float
                      }

_pprintV3 :: RealFloat a => V3 a -> String
_pprintV3 v = (foldMap (showFFloat (Just 3)) v) ", "

-- | Rotates the camera around the origin prom p1' to p2' on a unit sphere
--
--   Simply provide the x-y-z coordinates of a starting and stopping point, and
--   this should do the rest
rotateCamera :: IORef Camera -> (V3 Float) -> (V3 Float) -> IO()
rotateCamera ioCam p1 p2 = do
    (ArcBall rad rot) <- readIORef ioCam
    let p1'     = normalize p1
        p2'     = normalize p2
        n       = p1' `cross` p2'
        theta   = acos (p1' `dot` p2')
        w       = cos (theta / 2)
        v       =  (sin (theta / 2)) *^ n
        quat    = Quaternion w v
    writeIORef ioCam (ArcBall rad (rot * quat))

--moveCamera :: IORef Camera -> Float -> Float -> IO ()
--moveCamera ioCam yaw pitch = do
    --cam <- readIORef ioCam
    --let pos =  getPosition cam
        --dir =  getDirection cam
        --up  =  getUp cam
        --pitchAxis = (pos - dir) `cross` up
        --yawRot = axisAngle up yaw
        --pitchRot = axisAngle pitchAxis pitch
        --totalRot = (pitchRot * yawRot)
        --pos'   = rotate totalRot pos
        --up'    = rotate pitchRot up
        --newCam = LookAt { getPosition = pos'
                        --, getUp = up'
                        --, getDirection = getDirection cam
                        --, getLastPitchAxis = pitchAxis}
    ----putStrLn $ "pos' = " <> pprintV3 pos'
    ----putStrLn $ "    pos = " <> pprintV3 pos
    ----putStrLn $ "    dir = " <> pprintV3 dir
    ----putStrLn $ "    up = " <> pprintV3 up
    ----putStrLn $ "    up' = " <> pprintV3 up'
    ----putStrLn $ "    pitchAxis = " <> pprintV3 pitchAxis
    ----putStrLn $ "    radians = " <> show pitch
    ----putStrLn $ "    totalRot =" <> show totalRot
    --writeIORef ioCam newCam
