module Main (main) where
-- base
import Control.Monad (forever)
import Control.Concurrent (forkIO)

-- third party
import qualified Graphics.UI.GLFW as GLFW
import Linear.V3 (V3(..))

-- internal
import GLFW_Helpers (Window(..)
                    , glfwInit, closeIfNeeded, shutdownGLFW
                    , getCameraData, releaseContext, takeContext)
import ViewSpace (CameraData(..))
import GL_Renderer (Renderer, initRenderer, render, updateView, addObject)
import GL_Primitives (makeLine)

winWIDTH      = 800
winHEIGHT     = 600
winASPECT     = (fromIntegral winWIDTH) / (fromIntegral winHEIGHT)
winTITLE      = "LearnOpenGL Hello CAD!"

main :: IO ()
main = do
    putStrLn "executing main"
    mWindow <- glfwInit winWIDTH winHEIGHT winTITLE startCam

    maybe initFailMsg act mWindow

act :: Window -> IO()
act window = do
    -- initialize our renderer
    renderer <- initRenderer startCam winASPECT 5

    -- Make a few lines - this is for testing
    renderer' <- addObject renderer  (makeLine (V3 0 0 0)    (V3 10 10 10))
                 >>= (flip addObject (makeLine (V3 10 10 10) (V3 20 10 10)))
                 >>= (flip addObject (makeLine (V3 20 10 10) (V3 20 0 10)))
                 >>= (flip addObject (makeLine (V3 20 0 10)  (V3 20 0 0)))
                 >>= (flip addObject (makeLine (V3 20 0 0)   (V3 0 0 0)))

    -- Initial render
    updateView startCam renderer'
    render window renderer'

    -- Check camera queue in  different thread
    releaseContext
    forkIO $ checkCameraQueue renderer' window

    -- enter our main loop
    loop window renderer'

    -- Just in case our loop didn't manage to get there
    shutdownGLFW

loop :: Window -> Renderer -> IO ()
loop window renderer = do
    closeIfNeeded window
    GLFW.pollEvents

    loop window renderer

checkCameraQueue :: Renderer -> Window -> IO ()
checkCameraQueue renderer window = do
    takeContext window
    forever $ do
        cameraDatas  <- getCameraData window
        sequence_ $ fmap (flip updateView renderer) cameraDatas
        render window renderer

-------------------------------------------------------------------------------
--                    Consider moving this stuff elsewhere
-------------------------------------------------------------------------------

-- | This will initialize the camera.
startCam :: CameraData
startCam = LookAt { location  = V3 0 0 100 -- Where is the camera located
                  , up        = V3 0 1 0   -- Which way is "up" to the camera
                  , direction = V3 0 0 0   -- Where is it looking
                  }

-- | This message provides some useful output in case we can't initialize
initFailMsg :: IO ()
initFailMsg = do
    putStrLn "Failed to create a GLFW window!"
    putStrLn "  are you sure glfw is installed?"
    putStrLn "  If you're using Intel, you may need to enable software rendering"
    putStrLn "  If you're using a terminal, you may need to set DISPLAY."

