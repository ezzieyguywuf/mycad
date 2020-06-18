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
import Control.Monad (unless)

data Camera = LookAt {
                       location  :: V3 Float
                     , up        :: V3 Float
                     , direction :: V3 Float
                     }

_pprintV3 :: RealFloat a => V3 a -> String
_pprintV3 v = (foldMap (showFFloat (Just 3)) v) ", "

-- | Rotates the camera.
rotateCameraNudge :: IORef Camera
                  -> Float         -- ^ dx
                  -> Float         -- ^ dy
                  -> IO ()
rotateCameraNudge ioCam yaw pitch = do
    (LookAt loc up dir) <- readIORef ioCam
    let p1 = normalize loc
        p2 = normalize $ rotate (pitchRot * yawRot) p1
        right = (loc - dir) `cross` up
        yawRot = axisAngle up yaw
        pitchRot = axisAngle right (-pitch)
        axis = p1 `cross` p2
        theta = acos (p1 `dot` p2)
        rot = axisAngle axis theta
        loc' = rotate rot loc
        up'  = rotate rot up

    writeIORef ioCam (LookAt loc' up' dir)

zoomCamera :: IORef Camera -> Float -> IO ()
zoomCamera ioCam amt = do
    (LookAt loc up dir) <- readIORef ioCam
    let rad  = norm (loc - dir)
        rad' = rad - amt
        loc' = lerp (rad' / rad) loc dir
    unless (rad' <= 0.001) (writeIORef ioCam (LookAt loc' up dir))
