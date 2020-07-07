{-# LANGUAGE LambdaCase #-}
module Main (main) where
-- base
import Control.Monad (join, when)
import Control.Concurrent (threadDelay)

-- third party
import qualified Graphics.UI.GLFW as GLFW
import Linear.V3 (V3(..))

-- internal
import GLFW_Helpers (Window(..)
                    , glfwInit, shouldClose, shutdownGLFW
                    , hasNewCameraData, getCameraData)
import ViewSpace (CameraData(..))
import GL_Renderer (Renderer, initRenderer, render, updateView, addObject)
import GL_Primitives (makeLine)

winWIDTH      = 800
winHEIGHT     = 600
winASPECT     = fromIntegral winWIDTH / fromIntegral winHEIGHT
winTITLE      = "LearnOpenGL Hello CAD!"

main :: IO ()
main = do
    putStrLn "executing main"

    join getLoop

getLoop :: IO (IO ())
getLoop = do
    let lineThickness = 3

    glfwInit winWIDTH winHEIGHT winTITLE startCam >>= \case
        Nothing -> pure initFailMsg
        Just window -> do renderer <- initRenderer window startCam winASPECT lineThickness
                          debuggingLines renderer
                          pure (loop window renderer)

-- Make a few lines - this is for testing. This should be a wireframe cube
-- (sort of)
debuggingLines :: Renderer -> IO Renderer
debuggingLines renderer =
    -- Two basic lines
    --addObject renderer (makeLine (V3 0 0 0 ) (V3 10 10 10))
    -- >>= (flip addObject (makeLine (V3 10 10 10) (V3 10 20 10)))
    -- cube below
    addObject renderer  (makeLine (V3 (-10) (-10) (-10))    (V3 10  (-10)  (-10)))
     >>= (`addObject` (makeLine (V3 10  (-10)  (-10)) (V3 10 10  (-10))))
     >>= (`addObject` (makeLine (V3 10 10  (-10)) (V3  (-10) 10  (-10))))
     >>= (`addObject` (makeLine (V3  (-10) 10  (-10)) (V3  (-10)  (-10) (-10))))
     >>= (`addObject` (makeLine (V3  (-10)  (-10) (-10)) (V3  (-10) (-10) 10)))
     >>= (`addObject` (makeLine (V3 10 (-10) (-10))  (V3 10 (-10) 10)))
     >>= (`addObject` (makeLine (V3 10 10 (-10))  (V3 10 10 10)))
     >>= (`addObject` (makeLine (V3  (-10) 10 (-10))  (V3  (-10) 10 10)))
     >>= (`addObject` (makeLine (V3  (-10)  (-10) 10) (V3 10  (-10) 10)))
     >>= (`addObject` (makeLine (V3 10 (-10) 10) (V3 10 10 10)))
     >>= (`addObject` (makeLine (V3 10 10 10) (V3  (-10) 10 10)))
     >>= (`addObject` (makeLine (V3 (-10) 10 10) (V3  (-10) (-10) 10)))

loop :: Window -> Renderer -> IO ()
loop window renderer = do
    shouldClose window >>= \case
        False -> do GLFW.pollEvents
                    processCameraQueue renderer window
                    -- 1,000 microseconds = 1 millisecond (threadDelay takes microseconds)
                    threadDelay 100
                    loop window renderer
        True -> shutdownGLFW

processCameraQueue :: Renderer -> Window -> IO ()
processCameraQueue renderer window = do
    -- Only process the CameraQueue if there is data to process.
    hasNewCameraData window >>=
        (`when` do cameraDatas  <- getCameraData window
                   mapM_ (`updateView` renderer) cameraDatas
                   render window renderer
        )

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

