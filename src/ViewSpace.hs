module ViewSpace
( Camera(..)
, moveCamera
)where

import Linear.V3
import Linear.Quaternion
import Data.IORef
import Numeric

data Camera = LookAt { getPosition      :: V3 Float
                     , getUp            :: V3 Float
                     , getDirection     :: V3 Float
                     , getLastPitchAxis :: V3 Float
                     }

_pprintV3 :: RealFloat a => V3 a -> String
_pprintV3 v = (foldMap (showFFloat (Just 3)) v) ", "

moveCamera :: IORef Camera -> Float -> Float -> IO ()
moveCamera ioCam yaw pitch = do
    cam <- readIORef ioCam
    let pos =  getPosition cam
        dir =  getDirection cam
        up  =  getUp cam
        pitchAxis = (pos - dir) `cross` up
        yawRot = axisAngle (V3 0 0 1) yaw
        pitchRot = axisAngle pitchAxis pitch
        totalRot = (pitchRot * yawRot)
        pos'   = rotate totalRot pos
        up'    = rotate totalRot up
        newCam = LookAt { getPosition = pos'
                        , getUp = up'
                        , getDirection = getDirection cam
                        , getLastPitchAxis = pitchAxis}
    writeIORef ioCam newCam
