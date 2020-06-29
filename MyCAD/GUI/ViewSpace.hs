module ViewSpace
( Camera(..)
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
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, readTMVar, swapTMVar, putTMVar)
import Numeric

import Linear.Projection (lookAt, perspective)
import Control.Monad (unless)
import GL_Helpers (Shader(..), matrixUniform, putUniform)

data Camera = LookAt {
                       location  :: V3 Float
                     , up        :: V3 Float
                     , direction :: V3 Float
                     }

_pprintV3 :: RealFloat a => V3 a -> String
_pprintV3 v = (foldMap (showFFloat (Just 3)) v) ", "

initCamera :: TMVar Camera -> IO ()
initCamera cam = do
    atomically $ putTMVar cam mat
    where mat =LookAt { location  = V3 0 0 100
                        , up        = V3 0 1 0
                        , direction = V3 0 0 0
                      }

-- | Rotates the camera.
rotateCameraNudge :: TMVar Camera
                  -> Float         -- ^ dx
                  -> Float         -- ^ dy
                  -> IO ()
rotateCameraNudge cam yaw pitch = do
    (LookAt loc up dir) <- atomically $ readTMVar cam
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

    atomically $ swapTMVar cam (LookAt loc' up' dir)
    pure ()

zoomCamera :: TMVar Camera -> Float -> IO ()
zoomCamera camera amt = do
    (LookAt loc up dir) <- atomically $ readTMVar camera
    let rad  = norm (loc - dir)
        rad' = rad - amt
        loc' = lerp (rad' / rad) loc dir

    unless (rad' <= 0.001) (atomically $ swapTMVar camera (LookAt loc' up dir) >> pure ())

putViewUniform :: TMVar Camera -> [Shader] -> IO ()
putViewUniform camera shaders = do
    (LookAt loc up dir) <- atomically $ readTMVar camera
    mat <- matrixUniform (lookAt loc dir up) "view"
    sequence_ $ fmap (flip putUniform mat) shaders

putProjectionUniform :: Float -> Shader -> IO ()
putProjectionUniform aspect shader = matrixUniform projectionMatrix "projection" >>= (putUniform shader)
    where projectionMatrix = perspective (pi/4.0) aspect 0.1 1000.0
