module ViewSpace
( Camera
, initCamera
, rotateCameraNudge
, zoomCamera
, putViewUniform
, putProjectionUniform
)where

import Linear.V3
import Linear.Quaternion
import Linear.Metric
import Linear.Vector
import Numeric

import Linear.Projection (lookAt, perspective)
import Control.Monad (unless)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GL_Helpers (Shader, matrixUniform, putUniform)

type Camera = IORef CameraData


data CameraData = LookAt { location  :: V3 Float
                         , up        :: V3 Float
                         , direction :: V3 Float
                         }

_pprintV3 :: RealFloat a => V3 a -> String
_pprintV3 v = (foldMap (showFFloat (Just 3)) v) ", "

initCamera :: IO Camera
initCamera = do
    let cam = LookAt { location  = V3 0 0 100
                     , up        = V3 0 1 0
                     , direction = V3 0 0 0
                     }
    newIORef cam

-- | Rotates the camera.
rotateCameraNudge :: Camera
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

zoomCamera :: Camera -> Float -> IO ()
zoomCamera ioCam amt = do
    (LookAt loc up dir) <- readIORef ioCam
    let rad  = norm (loc - dir)
        rad' = rad - amt
        loc' = lerp (rad' / rad) loc dir

    unless (rad' <= 0.001) (writeIORef ioCam (LookAt loc' up dir))

putViewUniform :: Camera -> [Shader] -> IO ()
putViewUniform ioCam shaders = do
    (LookAt loc up dir) <- readIORef ioCam
    mat <- matrixUniform (lookAt loc dir up) "view"
    sequence_ $ fmap (flip putUniform mat) shaders


putProjectionUniform :: Float -> Shader -> IO ()
putProjectionUniform aspect shader = matrixUniform projectionMatrix "projection" >>= (putUniform shader)
    where projectionMatrix = perspective (pi/4.0) aspect 0.1 1000.0
