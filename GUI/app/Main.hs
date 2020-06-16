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

            cubeDrawer <- makeDrawer baseShader cube
            lineDrawer' <- makeDrawer baseShader line
            lineDrawer <- makeDrawer lineShader line'

            -- enable depth testing
            glEnable GL_DEPTH_TEST

            -- enter our main loop
            let loop = do
                    shouldContinue <- not <$> GLFW.windowShouldClose window
                    when shouldContinue $ do
                        -- event poll
                        GLFW.waitEvents
                        -- drawing
                        --   Background
                        glClearColor 0.2 0.3 0.3 1.0
                        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

                        --time <- maybe 0 realToFrac <$> GLFW.getTime
                        --moveCamera camera 0 (sin (time/100))

                        -- draw a cube
                        putViewUniform camera cubeDrawer
                        putProjectionUniform cubeDrawer
                        drawObject cubeDrawer

                        -- Draw the lines
                        putViewUniform camera lineDrawer'
                        putProjectionUniform lineDrawer'
                        drawObject lineDrawer'

                        -- Use our second shader program
                        putViewUniform camera lineDrawer
                        putProjectionUniform lineDrawer
                        drawObject lineDrawer

                        -- swap buffers and go again
                        GLFW.swapBuffers window
                        loop
            loop
    GLFW.terminate

putViewUniform :: IORef Camera -> Drawer -> IO ()
putViewUniform ioCam drawer = do
    (LookAt loc up dir) <- readIORef ioCam
    matrixUniform (lookAt loc dir up) "view" >>= (putUniform drawer)

putProjectionUniform :: Drawer -> IO ()
putProjectionUniform drawer = matrixUniform projectionMatrix "projection" >>= (putUniform drawer)
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
