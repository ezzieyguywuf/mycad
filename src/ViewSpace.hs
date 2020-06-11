module ViewSpace
( Camera(..)
, moveCamera
)where

import Data.Foldable
import Linear.V3
import Data.IORef

data Camera = LookAt { getPosition  :: V3 Float
                     , getUp        :: V3 Float
                     , getDirection :: V3 Float
                     }

moveCamera :: IORef Camera -> Float -> Float -> IO ()
moveCamera ioCam yaw pitch = do
    cam <- readIORef ioCam
    let [x, y, z'] = toList $ getPosition cam
        x' = x * (cos yaw) - y * (sin yaw)
        y' = x * (sin yaw) + y * (cos yaw)
        newCam = LookAt {getPosition = (V3 x' y' z'),
                         getUp = getUp cam,
                         getDirection = getDirection cam}
    writeIORef ioCam newCam
