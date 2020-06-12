module ViewSpace
( Camera(..)
, moveCamera
)where

import Linear.V3
import Linear.Quaternion
import Data.IORef
import Numeric

data Camera = LookAt { getPosition  :: V3 Float
                     , getUp        :: V3 Float
                     , getDirection :: V3 Float
                     }

pprintV3 :: RealFloat a => V3 a -> String
pprintV3 v = (foldMap (showFFloat (Just 3)) v) ", "

moveCamera :: IORef Camera -> Float -> Float -> IO ()
moveCamera ioCam yaw pitch = do
    cam <- readIORef ioCam
    let pos =  getPosition cam
        dir =  getDirection cam
        up  =  getUp cam
        pitchAxis = (pos - dir) `cross` up
        yawRot = axisAngle (V3 0 0 1) 0
        pitchRot = axisAngle pitchAxis pitch
        totalRot = (pitchRot * yawRot)
        pos'   = rotate totalRot pos
        newCam = LookAt { getPosition = pos'
                        , getUp = getUp cam
                        , getDirection = getDirection cam}
    putStrLn $ "pos' = " <> pprintV3 pos'
    putStrLn $ "    pos = " <> pprintV3 pos
    putStrLn $ "    dir = " <> pprintV3 dir
    putStrLn $ "    up = " <> pprintV3 up
    putStrLn $ "    pitchAxis = " <> pprintV3 pitchAxis
    putStrLn $ "    radians = " <> show pitch
    putStrLn $ "    totalRot =" <> show totalRot
    writeIORef ioCam newCam
