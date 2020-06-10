-- base
import Control.Monad (when)
import Control.Exception (bracket)
import Data.Bits

-- GLFW-b, qualified for clarity
import qualified Graphics.UI.GLFW as GLFW

-- helpers that we wrote
import GLFW_Helpers
import GL_Helpers

-- gl, all types and funcs here will already start with "gl"
import Graphics.GL.Core33
import Graphics.GL.Types

-- For Linear algebra...but really, like vectors and matrices and quaternions
import Linear.V3
import Linear.Matrix
import qualified Linear.Quaternion as Quat
{-import Linear.Projection-}

main :: IO ()
main = bracket GLFW.init (const GLFW.terminate) $ \initWorked ->
    when initWorked act

--          positions  Texture Coords
vs :: [GLfloat]
vs = [  -- A cube
       -0.5, -0.5, -0.5,  0.0, 0.0
     ,  0.5, -0.5, -0.5,  1.0, 0.0
     ,  0.5,  0.5, -0.5,  1.0, 1.0
     ,  0.5,  0.5, -0.5,  1.0, 1.0
     , -0.5,  0.5, -0.5,  0.0, 1.0
     , -0.5, -0.5, -0.5,  0.0, 0.0

     , -0.5, -0.5,  0.5,  0.0, 0.0
     ,  0.5, -0.5,  0.5,  1.0, 0.0
     ,  0.5,  0.5,  0.5,  1.0, 1.0
     ,  0.5,  0.5,  0.5,  1.0, 1.0
     , -0.5,  0.5,  0.5,  0.0, 1.0
     , -0.5, -0.5,  0.5,  0.0, 0.0

     , -0.5,  0.5,  0.5,  1.0, 0.0
     , -0.5,  0.5, -0.5,  1.0, 1.0
     , -0.5, -0.5, -0.5,  0.0, 1.0
     , -0.5, -0.5, -0.5,  0.0, 1.0
     , -0.5, -0.5,  0.5,  0.0, 0.0
     , -0.5,  0.5,  0.5,  1.0, 0.0

     ,  0.5,  0.5,  0.5,  1.0, 0.0
     ,  0.5,  0.5, -0.5,  1.0, 1.0
     ,  0.5, -0.5, -0.5,  0.0, 1.0
     ,  0.5, -0.5, -0.5,  0.0, 1.0
     ,  0.5, -0.5,  0.5,  0.0, 0.0
     ,  0.5,  0.5,  0.5,  1.0, 0.0

     , -0.5, -0.5, -0.5,  0.0, 1.0
     ,  0.5, -0.5, -0.5,  1.0, 1.0
     ,  0.5, -0.5,  0.5,  1.0, 0.0
     ,  0.5, -0.5,  0.5,  1.0, 0.0
     , -0.5, -0.5,  0.5,  0.0, 0.0
     , -0.5, -0.5, -0.5,  0.0, 1.0

     , -0.5,  0.5, -0.5,  0.0, 1.0
     ,  0.5,  0.5, -0.5,  1.0, 1.0
     ,  0.5,  0.5,  0.5,  1.0, 0.0
     ,  0.5,  0.5,  0.5,  1.0, 0.0
     , -0.5,  0.5,  0.5,  0.0, 0.0
     , -0.5,  0.5, -0.5,  0.0, 1.0
     ]

inds :: [GLuint]
inds  = [ 0, 1, 2
        , 2, 3, 0]

winWIDTH = 800
winHEIGHT = 600
winTITLE = "LearnOpenGL Hello Triangle"

act :: IO()
act = do
    maybeWindow <- glfwInit winWIDTH winHEIGHT winTITLE
    case maybeWindow of
        Nothing -> initFailMsg
        Just window -> do
            glfwWindowInit window

            -- Compile and like our shaders
            vshader <- readFile "./src/VertexShader.glsl"
                       >>= loadShader GL_VERTEX_SHADER
            fshader <- readFile "./src/FragmentShader.glsl"
                       >>= loadShader GL_FRAGMENT_SHADER

            shaderProgram <- linkShadersToProgram vshader fshader

            -- I guess these aren't needed any more?
            glDeleteShader vshader
            glDeleteShader fshader

            vao <- makeVertices vs inds

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

                        let rot   = fromQuaternion $ Quat.axisAngle (V3 1.0 (-1.0) 0.0) (-1 * pi/3)
                            trans = V3 0.0 0.0 0.0
                            scale = 1.0
                            model = transpose $ mkTransformationMat (scale *!! rot) trans
                        putMatrix shaderProgram model "model"
                        putMatrix shaderProgram (identity :: M44 Float) "view"

                        -- draw the triangle.
                        glBindVertexArray vao
                        glDrawArrays GL_TRIANGLES 0 36
                        glBindVertexArray 0

                        -- swap buffers and go again
                        GLFW.swapBuffers window
                        loop
            loop
    GLFW.terminate

