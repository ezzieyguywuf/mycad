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

data Camera = ArcBall { getPosition :: V3 Float
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
    cam@(ArcBall pos _) <- readIORef ioCam
    let p1@(V3 x y _) = pos
        x'    = x + dx
        y'    = y + dy
        z'    = getZ (norm pos) dx dy
        p1'   = normalize p1
        p2    = normalize $ (V3 x' y' z')
        n     = p1' `cross` p2
        theta = acos (p1' `dot` p2)
        rot  = axisAngle n theta
        rot' = axisAngle n (-theta)
        pos'  = rotate rot' pos
    writeIORef ioCam (ArcBall pos' rot)
    putStrLn $ "x',y',z' = " <> (show x') <> ", " <> (show y') <> ", " <> (show z')
    putStrLn $ "    pos = " <> (show pos)
    putStrLn $ "    dx = " <> (show dx) <> ", dy = " <> (show dy)
    putStrLn $ "    p1 = " <> (show p1)
    putStrLn $ "    p2 = " <> (show p2)
    putStrLn $ "    n = " <> (show n)
    putStrLn $ "    theta = " <> (show theta)
    putStrLn $ "    rot' = " <> (show rot')
    putStrLn $ "    pos' = " <> (show pos')

zoomCamera :: IORef Camera -> Float -> IO ()
zoomCamera ioCam amt = do
    (ArcBall pos rot) <- readIORef ioCam
    let rad  = norm pos
        rad' = rad - amt
        pos' = (rad' / rad) *^ pos
    writeIORef ioCam (ArcBall pos' rot)


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
