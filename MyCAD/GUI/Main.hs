module Main (main) where
-- base
import Data.Bits ((.|.))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar(tryTakeTMVar)
import System.FilePath ((</>))

-- third party
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Linear.V3 (V3(..))

-- internal
import GLFW_Helpers (Window(..), glfwInit, closeIfNeeded, shutdownGLFW,
                     swapBuffers)
import GL_Helpers ( Shader, Drawer, makeShader, makeObjectDrawer
                  , makeUniform, putUniform,  drawObject)
import VertexData (cube, line, circle)
import ViewSpace (CameraData(..), putProjectionUniform, putViewUniform)

winWIDTH      = 800
winHEIGHT     = 600
winASPECT     = (fromIntegral winWIDTH) / (fromIntegral winHEIGHT)
winTITLE      = "LearnOpenGL Hello CAD!"
vshaderFPATH  = "MyCAD" </> "GUI" </> "VertexShader.glsl"
lvshaderFPATH = "MyCAD" </> "GUI" </> "LineVShader.glsl"
fshaderFPATH  = "MyCAD" </> "GUI" </> "FragmentShader.glsl"

main :: IO ()
main = do
    putStrLn "executing main"
    mWindow <- glfwInit winWIDTH winHEIGHT winTITLE startCam

    maybe initFailMsg act mWindow

act :: Window -> IO()
act window = do
    (shaders, drawers) <- initShadersAndDrawers

    -- enter our main loop
    loop window shaders drawers

    -- Just in case our loop didn't manage to get there
    shutdownGLFW

loop :: Window -> [Shader] -> [Drawer] -> IO ()
loop window shaders drawers = do
    closeIfNeeded window
    GLFW.pollEvents

    -- Update our uniforms
    mTaken <- atomically $ tryTakeTMVar (cameraQueue window)
    case mTaken of
        Nothing         -> pure()
        Just cameraData -> do putViewUniform cameraData shaders
                              redraw window drawers
    loop window shaders drawers

redraw :: Window -> [Drawer] -> IO ()
redraw window drawers = do
    -- First, clear what was there
    glClearColor 0.2 0.3 0.3 1.0
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    -- draw it again
    sequence_ $ fmap drawObject drawers

    -- swap the buffers
    swapBuffers window

-------------------------------------------------------------------------------
--                    Consider moving this stuff elsewhere
-------------------------------------------------------------------------------

-- | This will initialize the camera.
startCam :: CameraData
startCam = LookAt { location  = V3 0 0 100 -- Where is the camera located
                  , up        = V3 0 1 0   -- Which way is "up" to the camera
                  , direction = V3 0 0 0   -- Where is it looking
                  }

initShadersAndDrawers :: IO ([Shader], [Drawer])
initShadersAndDrawers = do
    -- Compile and link our shaders
    baseShader <- makeShader vshaderFPATH fshaderFPATH
    lineShader <- makeShader lvshaderFPATH fshaderFPATH

    -- set static uniforms
    putProjectionUniform winASPECT baseShader
    putProjectionUniform winASPECT lineShader

    putUniform lineShader (makeUniform "aspect" winASPECT)
    putUniform lineShader (makeUniform "thickness" (5 :: Float))


    cubeDrawer <- makeObjectDrawer baseShader cube
    lineDrawer <- makeObjectDrawer lineShader line
    circleDrawer <- makeObjectDrawer lineShader circle

    pure ([baseShader, lineShader], [cubeDrawer, lineDrawer, circleDrawer])

-- | This message provides some useful output in case we can't initialize
initFailMsg :: IO ()
initFailMsg = do
    putStrLn "Failed to create a GLFW window!"
    putStrLn "  are you sure glfw is installed?"
    putStrLn "  If you're using Intel, you may need to enable software rendering"
    putStrLn "  If you're using a terminal, you may need to set DISPLAY."

