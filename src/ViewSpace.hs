module ViewSpace
( Camera(..)
, rotateCameraNudge
, zoomCamera
)where

import Linear.V3
import Linear.Quaternion
import Linear.Metric
import Linear.Vector
import Data.IORef
import Numeric

data Camera = LookAt { 
                       location  :: V3 Float
                     , up        :: V3 Float
                     , direction :: V3 Float
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
    (LookAt loc up dir) <- readIORef ioCam
    let p1@(V3 x1 y1 z1) = normalize loc
        p2 = V3 x2 y2 z2
        x2 = x1 + dx
        y2 = y1 + dy
        z2 = getZ 1 x2 y2
        axis = p1 `cross` p2
        theta = acos (p1 `dot` p2)
        w   = cos (theta / 2)
        v   = (sin (theta / 2)) *^ axis
        rot = Quaternion w v
        loc' = rotate rot loc
        up'  = rotate rot up
        
    writeIORef ioCam (LookAt loc' up' dir)

zoomCamera :: IORef Camera -> Float -> IO ()
zoomCamera ioCam amt = do
    (LookAt loc up dir) <- readIORef ioCam
    let rad  = norm (loc - dir)
        rad' = rad - amt
        loc' = lerp (rad' / rad) loc dir
    writeIORef ioCam (LookAt loc' up dir)


getZ :: Float -> Float -> Float -> Float
getZ rad x y = sqrt (rad**2 - x**2 - y**2)
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
