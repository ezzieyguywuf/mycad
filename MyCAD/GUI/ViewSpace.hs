module ViewSpace
(
  CameraData(..)
, rotateCameraNudge
, zoomCamera
, putViewUniform
, putProjectionUniform
)where

import Linear.V3 (V3(..), cross)
import Linear.Quaternion (axisAngle, rotate)
import Linear.Metric (normalize, dot, norm)
import Linear.Vector (lerp)

import Linear.Projection (lookAt, perspective)
import GL_Helpers (Shader, matrixUniform, putUniform)


-- | This is the actual data that our camera needs - this fully describes the
--   transformation needed to change the view
data CameraData = LookAt { location  :: V3 Float -- ^ Where is the camera located?
                         , up        :: V3 Float -- ^ which way is up for the camera?
                         , direction :: V3 Float -- ^ Which way is the camera looking?
                         }

-- | Rotates the camera.
--
--   The two-dimensionsal delta-x and delta-y are translated into a three-dimensional rotation by:
--
--       1. Normalizing the camera's current position to a unit sphere
--       2. Rotating the camera's position on the unit sphere by the @yaw@ and
--          @pitch@ provided to find the target point
--       3. Calculating the angle between the starting point and ending point
--       4. Calculating an axis-of-rotation perpendicular to both starting and ending points
--       5. Creating a quaternion from the angle and axis
--
--    The "Camera"s @loc@ value is updated to the new position, and it's @up@
--    vector is update in order to remain tangent to the sphere around which
--    the camera rotates (note: the camera itself does not rotate around a unit
--    sphere)
rotateCameraNudge :: CameraData
                  -> Float         -- ^ yaw
                  -> Float         -- ^ pitch
                  -> CameraData
rotateCameraNudge (LookAt loc up dir) yaw pitch = LookAt loc' up' dir
    where p1 = normalize loc
          p2 = normalize $ rotate (pitchRot * yawRot) p1
          right = (loc - dir) `cross` up
          yawRot = axisAngle up yaw
          pitchRot = axisAngle right (-pitch)
          axis = p1 `cross` p2
          theta = acos (p1 `dot` p2)
          rot = axisAngle axis theta
          loc' = rotate rot loc
          up'  = rotate rot up

-- | The zoom level is changed by moving the camera closer to its target.
--
--   There is a maximum zoom value using this method, due to floating point
--   precision and all that.
zoomCamera :: CameraData -> Float -> CameraData
zoomCamera oldCam@(LookAt loc up dir) amt = newCam
    where rad  = norm (loc - dir)
          rad' = rad - amt
          loc' = lerp (rad' / rad) loc dir
          newCam = if rad' <= 0.001
                      then oldCam
                      else LookAt loc' up dir

-- | Update the openGL \"Uniform\" matrix that specifies the View
putViewUniform :: CameraData -> [Shader] -> IO ()
putViewUniform (LookAt loc up dir) shaders = do
    mat <- matrixUniform (lookAt loc dir up) "view"
    sequence_ $ fmap (flip putUniform mat) shaders


-- | Update the openGL \"Uniform\" matrix that specifies the Projection
putProjectionUniform :: Float -> Shader -> IO ()
putProjectionUniform aspect shader = matrixUniform projectionMatrix "projection" >>= (putUniform shader)
    where projectionMatrix = perspective (pi/4.0) aspect 0.1 1000.0
