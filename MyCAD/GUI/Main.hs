{-# LANGUAGE LambdaCase #-}
module Main (main) where
-- base
import Control.Concurrent (threadDelay, forkIO)

-- third party
import qualified Graphics.UI.GLFW as GLFW
import Linear.V3 (V3(..))

-- internal
import GLFW_Helpers (Window, glfwInit, shutdownGLFW, shouldClose)
import ViewSpace (CameraData(..))
import GL_Renderer (Renderer, initRenderer
                   , queueObject, renderIfNecessary)
import GL_Primitives (makeLine)

winWIDTH      = 800
winHEIGHT     = 600
winASPECT     = fromIntegral winWIDTH / fromIntegral winHEIGHT
winTITLE      = "LearnOpenGL Hello CAD!"

main :: IO ()
main = do
    putStrLn "executing main"

    getRenderer >>= \case
        Nothing       -> pure ()
        Just (window, renderer) -> do forkIO (debuggingLines renderer)
                                      loop window renderer

getRenderer :: IO (Maybe (Window, Renderer))
getRenderer = do
    let lineThickness = 3

    -- TODO do we really ned startCam twice?
    glfwInit winWIDTH winHEIGHT winTITLE startCam >>= \case
        Nothing -> initFailMsg >> pure Nothing
        Just window -> do renderer <- initRenderer startCam winASPECT lineThickness
                          pure $ Just (window, renderer)

-- Make a few lines - this is for testing. This should be a wireframe cube
-- (sort of)
debuggingLines :: Renderer -> IO ()
debuggingLines renderer = do
    -- Two basic lines
    --addObject renderer (makeLine (V3 0 0 0 ) (V3 10 10 10))
    -- >>= (flip addObject (makeLine (V3 10 10 10) (V3 10 20 10)))
    -- cube below
    queueObject renderer (makeLine (V3 (-10) (-10) (-10))    (V3 10  (-10)  (-10)))
    queueObject renderer (makeLine (V3 10  (-10)  (-10)) (V3 10 10  (-10)))
    queueObject renderer (makeLine (V3 10 10  (-10)) (V3  (-10) 10  (-10)))
    queueObject renderer (makeLine (V3  (-10) 10  (-10)) (V3  (-10)  (-10) (-10)))
    queueObject renderer (makeLine (V3  (-10)  (-10) (-10)) (V3  (-10) (-10) 10))
    queueObject renderer (makeLine (V3 10 (-10) (-10))  (V3 10 (-10) 10))
    queueObject renderer (makeLine (V3 10 10 (-10))  (V3 10 10 10))
    queueObject renderer (makeLine (V3  (-10) 10 (-10))  (V3  (-10) 10 10))
    queueObject renderer (makeLine (V3  (-10)  (-10) 10) (V3 10  (-10) 10))
    queueObject renderer (makeLine (V3 10 (-10) 10) (V3 10 10 10))
    queueObject renderer (makeLine (V3 10 10 10) (V3  (-10) 10 10))
    queueObject renderer (makeLine (V3 (-10) 10 10) (V3  (-10) (-10) 10))

loop :: Window -> Renderer -> IO ()
loop window renderer = do
    shouldClose window >>= \case
        False -> do
            -- Might trigger a render
            GLFW.pollEvents

            -- Only renders when something has triggered a render
            renderIfNecessary renderer

            -- So we don't use up all the CPU.  100 microseconds = 0.1 millisecond
            -- (threadDelay takes microseconds)
            threadDelay 100

            -- Go again!
            loop window renderer
        True -> shutdownGLFW

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
