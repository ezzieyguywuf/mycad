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
import Linear.Matrix
import qualified Linear.Quaternion as Quat
import Linear.Projection

import Foreign
import Data.IORef

main :: IO ()
main = bracket GLFW.init (const GLFW.terminate) $ \initWorked ->
    when initWorked act

winWIDTH = 800
winHEIGHT = 600
winTITLE = "LearnOpenGL Hello Cube!"

act :: IO()
act = do
    maybeWindow <- glfwInit winWIDTH winHEIGHT winTITLE
    case maybeWindow of
        Nothing -> initFailMsg
        Just window -> do
            -- Set up some...well global variables
            camera <- newIORef initCamera

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

            vao2 <- putGraphicData cube cubeIndices

            -- Load the texture information into opengl
            t1 <- loadTexture "res/container.jpg"
            t2 <- loadTexture "res/awesomeface.png"

            mapTextureUnit shaderProgram 0 "texture0"
            mapTextureUnit shaderProgram 1 "texture1"

            -- enable depth testing
            glEnable GL_DEPTH_TEST

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

                        -- Use our program
                        glUseProgram shaderProgram

                        -- Bind the texture we want to use
                        -- Tell openGL about our Uniforms
                        glActiveTexture GL_TEXTURE0
                        glBindTexture GL_TEXTURE_2D t1

                        glActiveTexture GL_TEXTURE1
                        glBindTexture GL_TEXTURE_2D t2

                        placeModel shaderProgram
                        placeCamera shaderProgram camera
                        makeProjection shaderProgram

                        -- Draw the first cube
                        glBindVertexArray vao2
                        let len = fromIntegral $ length cubeIndices
                        glDrawElements GL_TRIANGLES len GL_UNSIGNED_INT nullPtr
                        glBindVertexArray 0

                        -- swap buffers and go again
                        GLFW.swapBuffers window
                        loop
            loop
    GLFW.terminate

placeModel :: GLuint -> IO ()
placeModel shaderProgram = do
    let theta = 0.0
        rot   = fromQuaternion $ Quat.axisAngle (V3 1.0 (-1.0) 0.0) theta
        trans = V3 0.0 0.0 0.0
        scale = 1.0
        model = mkTransformationMat (scale *!! rot) trans
    putMatrix shaderProgram model "model"

placeCamera :: GLuint -> IORef Camera -> IO ()
placeCamera shaderProgram ioCam = do
    time <- maybe 0 realToFrac <$> GLFW.getTime
    camera <- readIORef ioCam
    let eye    = getPosition camera
        center = getDirection camera
        up     = getUp camera
    putMatrix shaderProgram  (lookAt eye center up) "view"

makeProjection :: GLuint -> IO ()
makeProjection shaderProgram = do
    let aspectRatio = (fromIntegral winWIDTH) / (fromIntegral winHEIGHT)
        projection = infinitePerspective (pi/4.0) aspectRatio 0.1
    putMatrix shaderProgram projection "projection"

initCamera :: Camera
initCamera = LookAt { getPosition = (V3 0 (-35) (-45))
                    , getUp  = (V3 0 0 1)
                    , getDirection = (V3 0 0 0)
                    }
