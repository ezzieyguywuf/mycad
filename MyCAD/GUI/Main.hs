module Main (main) where
-- base
import Control.Monad (unless)
import Data.Bits

-- GLFW-b, qualified for clarity
import qualified Graphics.UI.GLFW as GLFW

-- helpers that we wrote
import GLFW_Helpers
import GL_Helpers
import VertexData
import ViewSpace

-- gl, all types and funcs here will already start with "gl"
import Graphics.GL.Core33

-- For Linear algebra...but really, like vectors and matrices and quaternions
import Linear.V3
import Linear.V4
import Linear.Projection
import Linear.Quaternion

import Data.IORef

main :: IO ()
main = do
    -- Set up some...well global variables
    camera <- initCamera

    mWindow <- (glfwInit camera) winWIDTH winHEIGHT winTITLE

    maybe initFailMsg (act camera) mWindow

winWIDTH      = 800
winHEIGHT     = 600
winASPECT     = (fromIntegral winWIDTH) / (fromIntegral winHEIGHT)
winTITLE      = "LearnOpenGL Hello CAD!"
vshaderFPATH  = "MyCAD/GUI/VertexShader.glsl"
lvshaderFPATH = "MyCAD/GUI/LineVShader.glsl"
fshaderFPATH  = "MyCAD/GUI/FragmentShader.glsl"

act :: IORef Camera -> GLFW.Window -> IO()
act camera window = do
    -- Compile and like our shaders
    baseShader <- makeShader vshaderFPATH fshaderFPATH
    lineShader <- makeShader lvshaderFPATH fshaderFPATH

    cubeDrawer <- makeObjectDrawer baseShader cube
    lineDrawer' <- makeObjectDrawer baseShader line'
    circleDrawer <- makeObjectDrawer lineShader circle

    -- set static uniforms
    putProjectionUniform baseShader
    putProjectionUniform lineShader

    floatUniform winASPECT "aspect" >>= putUniform lineShader
    floatUniform 5 "thickness" >>= putUniform lineShader

    -- jump down below to see the first call to loop
    let loop = do
            GLFW.windowShouldClose window >>= flip unless (do
                -- event poll
                GLFW.pollEvents
                -- drawing
                --   Background
                glClearColor 0.2 0.3 0.3 1.0
                glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

                -- Update our uniforms
                putViewUniform camera baseShader
                putViewUniform camera lineShader

                -- draw static objects
                drawObject cubeDrawer
                drawObject lineDrawer'
                drawObject circleDrawer

                -- Draw the rotating line
                time <- maybe 0 realToFrac <$> GLFW.getTime
                let p0 = V3 (-15) 15 0
                    p2 = V3 (-15) (-15) 0
                    axis = axisAngle (V3 0 0 1) (pi * ((sin time) + 1))
                    p1' = Linear.Quaternion.rotate (axis) (p0 - p2)
                    p1 = p1' + p2
                    col1 = V4 0.5 1.0 0.2 1.0
                cam <- readIORef camera
                lineDrawer2 <- makeObjectDrawer lineShader (makeLine' p1 p2 col1)

                drawObject lineDrawer2

                rotateCameraNudge camera (-0.005) 0

                -- swap buffers and go again
                GLFW.swapBuffers window
                loop
                )

    -- enter our main loop
    loop
    GLFW.terminate

putViewUniform :: IORef Camera -> Shader -> IO ()
putViewUniform ioCam shader = do
    (LookAt loc up dir) <- readIORef ioCam
    matrixUniform (lookAt loc dir up) "view" >>= (putUniform shader)


putProjectionUniform :: Shader -> IO ()
putProjectionUniform shader = matrixUniform projectionMatrix "projection" >>= (putUniform shader)
    where projectionMatrix = perspective (pi/4.0) winASPECT 0.1 1000.0

initCamera :: IO (IORef Camera)
initCamera = newIORef LookAt {
                               location  = V3 0 0 100
                             , up        = V3 0 1 0
                             , direction = V3 0 0 0
                             }
