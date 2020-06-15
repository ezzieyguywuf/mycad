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
            vshader <- readFile "./src/VertexShader.glsl"
                       >>= loadShader GL_VERTEX_SHADER
            fshader <- readFile "./src/FragmentShader.glsl"
                       >>= loadShader GL_FRAGMENT_SHADER

            shaderProgram <- linkShadersToProgram vshader fshader

            -- I guess these aren't needed any more?
            glDeleteShader vshader
            glDeleteShader fshader

            vao <- putGraphicData line (getIndices lineElements)
            vao2 <- putGraphicData cube (getIndices cubeElements)

            -- Load the texture information into opengl
            t1 <- loadTexture "./res/container.jpg"
            t2 <- loadTexture "./res/awesomeface.png"

            mapTextureUnit shaderProgram 0 "texture0"
            mapTextureUnit shaderProgram 1 "texture1"

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

                        -- Use our program
                        glUseProgram shaderProgram

                        -- Bind the texture we want to use
                        -- Tell openGL about our Uniforms
                        --glActiveTexture GL_TEXTURE0
                        --glBindTexture GL_TEXTURE_2D t1

                        --glActiveTexture GL_TEXTURE1
                        --glBindTexture GL_TEXTURE_2D t2

                        --time <- maybe 0 realToFrac <$> GLFW.getTime
                        --moveCamera camera 0 (sin (time/100))
                        placeCamera shaderProgram camera
                        makeProjection shaderProgram

                        -- Draw the lines
                        glBindVertexArray vao
                        let len = fromIntegral $ length (getIndices lineElements)
                            place = map (placeModel shaderProgram) (getGeoData lineElements)
                            draw  = map (\x -> x >> drawElements len) place
                        sequence_ $ draw

                        -- Draw same lines, offset down in y a bit
                        glBindVertexArray vao
                        let (GeoData inds datas) = lineElements
                            shift (ModelData rot (V3 x y z)) = ModelData rot (V3 x (y - 10) z)
                            datas' = map shift datas
                            len = fromIntegral $ length inds
                            place = map (placeModel shaderProgram) datas'
                            draw  = map (\x -> x >> drawElements len) place
                        sequence_ $ draw
                        glBindVertexArray 0

                        glBindVertexArray vao2
                        let len = fromIntegral $ length (getIndices cubeElements)
                            place = map (placeModel shaderProgram) (getGeoData cubeElements)
                            draw  = map (\x -> x >> drawElements len) place
                        sequence_ $ draw
                        glBindVertexArray 0

                        -- swap buffers and go again
                        GLFW.swapBuffers window
                        loop
            loop
    GLFW.terminate

drawElements :: GLsizei -> IO()
drawElements len = glDrawElements GL_TRIANGLES len GL_UNSIGNED_INT nullPtr

placeModel :: GLuint -> ModelData -> IO ()
placeModel shaderProgram dat = putMatrix shaderProgram (makeMatrix dat) "model"

placeCamera :: GLuint -> IORef Camera -> IO ()
placeCamera shaderProgram ioCam = do
    (LookAt loc up dir ) <- readIORef ioCam
    putMatrix shaderProgram  (lookAt loc dir up) "view"

makeProjection :: GLuint -> IO ()
makeProjection shaderProgram = do
    let aspectRatio = (fromIntegral winWIDTH) / (fromIntegral winHEIGHT)
        projection = perspective (pi/4.0) aspectRatio 0.1 1000.0
    putMatrix shaderProgram projection "projection"

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
