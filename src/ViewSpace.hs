module ViewSpace
( Camera(..)
, rotateCameraNudge
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

-- | Rotates the camera around the origin
--
--   Imagine looking straight down at a sphere and pinching the point closest
--   to you. Now move your fingers up/down left/right. This will cause the
--   sphere to rotate around its center. That's what this function does.
--
--   The maximum you'll be able to rotate the camera in such a manner is
--   90-degrees in any direction. This is intended to be called incrementally,
--   say when tracking a mouse pointer
rotateCameraNudge :: IORef Camera
                  -> Float         -- ^ dx
                  -> Float         -- ^ dy
                  -> IO ()
rotateCameraNudge ioCam dx dy = do
    cam@(ArcBall rad rot) <- readIORef ioCam
    let p1 = (V3 0 0 1)
        p2 = normalize $ (V3 dx dy 1)
        n       = p1 `cross` p2
        theta   = acos (p1 `dot` p2)
        quat    = axisAngle n theta
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
