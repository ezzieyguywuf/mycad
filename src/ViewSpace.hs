module ViewSpace
( Camera(..)
, moveCamera
)where

import Linear.V3
import Linear.Quaternion
import Data.IORef

data Camera = LookAt { getPosition  :: V3 Float
                     , getUp        :: V3 Float
                     , getDirection :: V3 Float
                     }

moveCamera :: IORef Camera -> Float -> Float -> IO ()
moveCamera ioCam yaw pitch = do
    cam <- readIORef ioCam
    let pos =  getPosition cam
        dir =  getDirection cam
        up  =  getUp cam
        pitchAxis = dir `cross` up
        yawRot = axisAngle (V3 0 0 1) yaw
        pitchRot = axisAngle pitchAxis pitch
        pos'   = rotate pitchRot (rotate yawRot pos)
        newCam = LookAt { getPosition = pos'
                        , getUp = getUp cam
                        , getDirection = getDirection cam}
    writeIORef ioCam newCam
