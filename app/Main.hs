-- base
import Control.Monad (when)
import Control.Exception (bracket)

-- GLFW-b, qualified for clarity
import qualified Graphics.UI.GLFW as GLFW

-- helpers that we wrote
import GLFW_Helpers
import GL_Helpers

-- gl, all types and funcs here will already start with "gl"
import Graphics.GL.Core33
import Graphics.GL.Types

-- This is for the Foreign Function Interface, ffi. This calls C-code
import Foreign
import Foreign.C.String (newCString)

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
vs = [  0.5,  0.5, 0.0, 1.0, 1.0  -- top right
     ,  0.5, -0.5, 0.0, 1.0, 0.0  -- bottom right
     , -0.5, -0.5, 0.0, 0.0, 0.0  -- bottom left
     , -0.5,  0.5, 0.0, 0.0, 1.0  -- top left
     ]

inds :: [GLuint]
inds  = [ 0, 1, 2
        , 2, 3, 0]

modelMatrix :: GLuint -> Quat.Quaternion Float -> V3 Float -> Float -> IO ()
modelMatrix shaderProgram rotation translation scale = do
    -- First, calculate the transformation matrix
    let rotM = fromQuaternion rotation :: M33 Float
        transMatrix = transpose $ mkTransformationMat (scale *!! rotM) translation

    -- Now do the openGL stuff
    transP <- malloc
    poke transP transMatrix

    name <- newCString "model"
    loc <- glGetUniformLocation shaderProgram name

    glUniformMatrix4fv loc 1 GL_FALSE (castPtr transP)

projectionMatrix :: GLuint -> M44 Float -> IO ()
projectionMatrix shaderProgram transMatrix = do
    transP <- malloc
    poke transP transMatrix

    name <- newCString "view"
    loc <- glGetUniformLocation shaderProgram name

    glUniformMatrix4fv loc 1 GL_FALSE (castPtr transP)

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

            -- enter our main loop
            let loop = do
                    shouldContinue <- not <$> GLFW.windowShouldClose window
                    when shouldContinue $ do
                        -- event poll
                        GLFW.pollEvents
                        -- drawing
                        --   Background
                        glClearColor 0.2 0.3 0.3 1.0
                        glClear GL_COLOR_BUFFER_BIT

                        -- Use our program
                        glUseProgram shaderProgram

                        -- Bind the texture we want to use
                        -- Tell openGL about our Uniforms
                        glActiveTexture GL_TEXTURE0
                        glBindTexture GL_TEXTURE_2D t1

                        glActiveTexture GL_TEXTURE1
                        glBindTexture GL_TEXTURE_2D t2

                        --                    rotation     translation      scale
                        let rot = Quat.axisAngle (V3 1.0 0.0 0.0) (-1 * pi/3)
                        modelMatrix shaderProgram rot (V3 0.0 0.0 0.0) 1.0
                        projectionMatrix shaderProgram (identity :: M44 Float)

                        -- draw the triangle.
                        glBindVertexArray vao
                        glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT nullPtr
                        glBindVertexArray 0

                        -- swap buffers and go again
                        GLFW.swapBuffers window
                        loop
            loop
    GLFW.terminate

