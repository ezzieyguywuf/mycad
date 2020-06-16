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
import Graphics.GL.Types

-- For Linear algebra...but really, like vectors and matrices and quaternions
import Linear.V3
import Linear.Projection
import Linear.Matrix

import Foreign
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
            lineShader <- makeShader' "./src/LineVShader.glsl" "./src/FragmentShader.glsl"

            vao <- putGraphicData line (getIndices lineElements)
            vao2 <- putGraphicData cube (getIndices cubeElements)
            lineDrawer <- makeDrawer lineShader line''

            -- Load the texture information into opengl
            t1 <- loadTexture "./res/container.jpg"
            t2 <- loadTexture "./res/awesomeface.png"

            mapTextureUnit baseShader 0 "texture0"
            mapTextureUnit baseShader 1 "texture1"

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

                        -- Bind the texture we want to use
                        -- Tell openGL about our Uniforms
                        --glActiveTexture GL_TEXTURE0
                        --glBindTexture GL_TEXTURE_2D t1

                        --glActiveTexture GL_TEXTURE1
                        --glBindTexture GL_TEXTURE_2D t2

                        --time <- maybe 0 realToFrac <$> GLFW.getTime
                        --moveCamera camera 0 (sin (time/100))
                        placeCamera baseShader camera
                        makeProjection baseShader

                        -- Use our program
                        glUseProgram baseShader

                        -- draw a cube
                        glBindVertexArray vao2
                        let len = fromIntegral $ length (getIndices cubeElements)
                            place = map (placeModel baseShader) (getGeoData cubeElements)
                            draw  = map (\x -> x >> drawElements len) place
                        sequence_ $ draw
                        glBindVertexArray 0

                        -- Draw the lines
                        glBindVertexArray vao
                        let len = fromIntegral $ length (getIndices lineElements)
                            place = map (placeModel baseShader) (getGeoData lineElements)
                            draw  = map (\x -> x >> drawElements len) place
                        sequence_ $ draw

                        -- Use our second shader program
                        --glUseProgram lineShader
                        viewMat <- viewMatrix camera
                        matrixUniform viewMat "view" >>= (putUniform' lineDrawer)
                        matrixUniform projectionMatrix "projection" >>= (putUniform' lineDrawer)

                        drawObject lineDrawer line''
                        -- swap buffers and go again
                        GLFW.swapBuffers window
                        loop
            loop
    GLFW.terminate

drawElements :: GLsizei -> IO()
drawElements len = glDrawElements GL_TRIANGLES len GL_UNSIGNED_INT nullPtr

placeModel :: GLuint -> ElementData -> IO ()
placeModel shader dat = putMatrix shader (makeMatrix dat) "model"

placeCamera :: GLuint -> IORef Camera -> IO ()
placeCamera shader ioCam = do
    mat <- viewMatrix ioCam
    putMatrix shader mat  "view"

makeProjection :: GLuint -> IO ()
makeProjection shader = do
    let aspectRatio = (fromIntegral winWIDTH) / (fromIntegral winHEIGHT)
        projection = perspective (pi/4.0) aspectRatio 0.1 1000.0
    putMatrix shader projection "projection"


projectionMatrix :: M44 Float
projectionMatrix = perspective (pi/4.0) aspectRatio 0.1 1000.0
    where aspectRatio = (fromIntegral winWIDTH) / (fromIntegral winHEIGHT)

viewMatrix :: IORef Camera -> IO (M44 Float)
viewMatrix ioCam = do
    (LookAt loc up dir ) <- readIORef ioCam
    pure $ lookAt loc dir up

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
