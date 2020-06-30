module Main (main) where
-- base
import Control.Monad (unless)
import Data.Bits ((.|.))

-- GLFW-b, qualified for clarity
import qualified Graphics.UI.GLFW as GLFW

-- helpers that we wrote
import GLFW_Helpers
import GL_Helpers
import VertexData
import ViewSpace

-- gl, all types and funcs here will already start with "gl"
import Graphics.GL.Core33

main :: IO ()
main = do
    mWindow <- glfwInit winWIDTH winHEIGHT winTITLE

    maybe initFailMsg act mWindow

winWIDTH      = 800
winHEIGHT     = 600
winASPECT     = (fromIntegral winWIDTH) / (fromIntegral winHEIGHT)
winTITLE      = "LearnOpenGL Hello CAD!"
vshaderFPATH  = "MyCAD/GUI/VertexShader.glsl"
lvshaderFPATH = "MyCAD/GUI/LineVShader.glsl"
fshaderFPATH  = "MyCAD/GUI/FragmentShader.glsl"

act :: Window -> IO()
act window = do
    -- Compile and link our shaders
    baseShader <- makeShader vshaderFPATH fshaderFPATH
    lineShader <- makeShader lvshaderFPATH fshaderFPATH

    cubeDrawer <- makeObjectDrawer baseShader cube
    lineDrawer <- makeObjectDrawer lineShader line
    circleDrawer <- makeObjectDrawer lineShader circle

    -- set static uniforms
    putProjectionUniform winASPECT baseShader
    putProjectionUniform winASPECT lineShader

    floatUniform winASPECT "aspect" >>= putUniform lineShader
    floatUniform 5 "thickness" >>= putUniform lineShader

    -- jump down below to see the first call to loop
    let loop = do
            bClose <- shouldClose window
            unless bClose $ do
                -- event poll
                GLFW.pollEvents

                -- drawing
                --   Background
                glClearColor 0.2 0.3 0.3 1.0
                glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

                -- Update our uniforms
                putViewUniform (camera window) [baseShader, lineShader]

                -- draw static objects
                sequence_ $ fmap drawObject [cubeDrawer, lineDrawer, circleDrawer]

                --rotateCameraNudge camera (-0.005) 0

                -- swap buffers and go again
                swapBuffers window
                loop

    -- enter our main loop
    loop
    GLFW.terminate
