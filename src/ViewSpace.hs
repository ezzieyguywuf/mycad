module ViewSpace
( Camera(..)
, moveCamera
)where

import Data.Foldable
import Linear.V3
import Data.IORef

data Camera = LookAt { getPosition  :: V3 Double
                     , getUp        :: V3 Double
                     , getDirection :: V3 Double
                     }

moveCamera :: IORef Camera -> Double -> Double -> IO ()
moveCamera ioCam yaw pitch = do
    cam <- readIORef ioCam
    let [_, _, z'] = toList $ getPosition cam
        x' = (cos yaw)
        y' = (sin yaw)
        newCam = LookAt {getPosition = (V3 x' y' z'),
                         getUp = getUp cam,
                         getDirection = getDirection cam}
    writeIORef ioCam newCam

