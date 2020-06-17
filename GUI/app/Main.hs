-- base
import Control.Monad (when)
import Control.Exception (bracket)
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
import Linear.Projection
import Linear.Quaternion

import Data.IORef

main :: IO ()
main = bracket GLFW.init (const GLFW.terminate) $ \initWorked ->
    when initWorked act

winWIDTH = 800
winHEIGHT = 600
winTITLE = "LearnOpenGL Hello Line!"

act :: IO()
act = do
    maybeWindow <- glfwInit winWIDTH winHEIGHT winTITLE
    case maybeWindow of
        Nothing -> initFailMsg
        Just window -> do
            -- Initialize console output
            initializeConsole

            -- Set up some...well global variables
            camera <- initCamera

            -- Initialize glfw things, including callbacks
            glfwWindowInit window camera

            -- Compile and like our shaders
            baseShader <- makeShader "./src/VertexShader.glsl" "./src/FragmentShader.glsl"
            lineShader <- makeShader "./src/LineVShader.glsl" "./src/FragmentShader.glsl"

            cubeDrawer <- makeObjectDrawer baseShader cube
            lineDrawer' <- makeObjectDrawer baseShader line'
            lineDrawer <- makeObjectDrawer lineShader line

            -- enable depth testing
            glEnable GL_DEPTH_TEST

            circleDrawer <- makeObjectDrawer lineShader circle

            -- set static uniforms
            putProjectionUniform baseShader
            putProjectionUniform lineShader

            -- draw static objects
            drawObject cubeDrawer
            drawObject lineDrawer'
            drawObject lineDrawer
            drawObject circleDrawer

            -- enter our main loop
            let loop = do
                    shouldContinue <- not <$> GLFW.windowShouldClose window
                    when shouldContinue $ do
                        -- event poll
                        GLFW.pollEvents
                        -- drawing
                        --   Background
                        glClearColor 0.2 0.3 0.3 1.0
                        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)


                        -- Update our uniforms
                        putViewUniform camera baseShader
                        putViewUniform camera lineShader

                        -- Draw the rotating line
                        time <- maybe 0 realToFrac <$> GLFW.getTime
                        let p0 = V3 (-15) 15 0
                            p2 = V3 (-15) (-15) 0
                            p1' = Linear.Quaternion.rotate (axisAngle (V3 0 0 1) (pi * ((sin time) + 1))) (p0 - p2)
                            p1 = p1' + p2
                            width = 3.0
                        cam <- readIORef camera
                        lineDrawer2 <- makeObjectDrawer lineShader (makeLine width p1 p2)

                        drawObject lineDrawer2

                        -- swap buffers and go again
                        GLFW.swapBuffers window
                        loop
            loop
    GLFW.terminate

putViewUniform :: IORef Camera -> Shader -> IO ()
putViewUniform ioCam shader = do
    (LookAt loc up dir) <- readIORef ioCam
    matrixUniform (lookAt loc dir up) "view" >>= (putUniform shader)

putProjectionUniform :: Shader -> IO ()
putProjectionUniform shader = matrixUniform projectionMatrix "projection" >>= (putUniform shader)
    where projectionMatrix = perspective (pi/4.0) aspectRatio 0.1 1000.0
          aspectRatio = (fromIntegral winWIDTH) / (fromIntegral winHEIGHT)

initCamera :: IO (IORef Camera)
initCamera = newIORef LookAt { 
                               location  = V3 0 0 100
                             , up        = V3 0 1 0
                             , direction = V3 0 0 0
                             }

initializeConsole :: IO ()
initializeConsole = do
    putStrLn "All data should update only below here. Welcome!"
    sequence_ $ take 5 (repeat $ putStrLn "")
