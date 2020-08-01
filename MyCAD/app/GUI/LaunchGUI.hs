{-# LANGUAGE LambdaCase #-}
{-|
Module      : LaunchGUI
Description : Launches MyCAD's Graphical User Interface
Copyright   : (c) Wolfgang E. Sanyer, 2020
License     : MPL2
Maintainer  : WolfgangESanyer@Gmail.com
Stability   : experimental
Portability : POSIX

This will launch MyCAD's Graphical User Interface. It provides an OpenGL-based
interface using Haskell bindings to the popular glfw library.

glfw seems very mature, as well as actively maintained. It is also a rather
lightweight dependency, as far as gui libraries are concerned.
-}
module GUI.LaunchGUI (initialize, launch) where

-- base
import Control.Concurrent (threadDelay, forkIO)

-- third party
import qualified Graphics.UI.GLFW as GLFW
import Linear.V3 (V3(..))

-- internal
import GUI.GLFW_Helpers (Window, glfwInit, shutdownGLFW, shouldClose)
import GUI.ViewSpace (CameraData(..), rotateCameraNudge)
import GUI.RenderQueue (RenderQueue, queueObject, initRenderQueue)
import GUI.GL.RenderData (RenderData, initRenderData)
import GUI.GL.Primitives (makeLine)
import GUI.GL.Renderer (renderIfNecessary)

winWIDTH :: Int
winWIDTH      = 800

winHEIGHT :: Int
winHEIGHT     = 600

winASPECT :: Float
winASPECT     = fromIntegral winWIDTH / fromIntegral winHEIGHT

winTITLE :: String
winTITLE      = "LearnOpenGL Hello CAD!"

initialize :: IO RenderQueue
initialize = initRenderQueue startCam

launch :: RenderQueue -> IO ()
launch queue = do
    let lineThickness = 3

    glfwInit winWIDTH winHEIGHT winTITLE queue startCam >>= \case
        Nothing -> initFailMsg
        Just window -> do
            renderData <- initRenderData queue winASPECT lineThickness
            forkIO (debuggingLines queue)
            loop window renderData

loop :: Window -> RenderData -> IO ()
loop window renderData =
    shouldClose window >>= \case
        False -> do
            -- Might trigger a render
            GLFW.pollEvents

            -- Only renders when something has triggered a render
            renderData' <- renderIfNecessary window renderData

            -- So we don't use up all the CPU.  100 microseconds = 0.1 millisecond
            -- (threadDelay takes microseconds)
            threadDelay 100

            -- Go again!
            loop window renderData'
        True -> shutdownGLFW

-------------------------------------------------------------------------------
--                    Consider moving this stuff elsewhere
-------------------------------------------------------------------------------

-- | This will initialize the camera.
startCam :: CameraData
startCam = rotateCameraNudge initialCam yaw pitch
    where initialCam = LookAt { getLoc = V3 0 0 100 -- Where is the camera located
                              , getUp  = V3 0 1 0   -- Which way is "up" to the camera
                              , getDir = V3 0 0 0   -- Where is it looking
                              }
          yaw = (-pi / 4.0)
          pitch = pi / 6

-- | This message provides some useful output in case we can't initialize
initFailMsg :: IO ()
initFailMsg = do
    putStrLn "Failed to create a GLFW window!"
    putStrLn "  are you sure glfw is installed?"
    putStrLn "  If you're using Intel, you may need to enable software rendering"
    putStrLn "  If you're using a terminal, you may need to set DISPLAY."

-- Make a few lines - this is for testing. This should be a wireframe cube
-- (sort of)
debuggingLines :: RenderQueue -> IO ()
debuggingLines queue = do
    -- Two basic lines
    --addObject renderer (makeLine (V3 0 0 0 ) (V3 10 10 10))
    -- >>= (flip addObject (makeLine (V3 10 10 10) (V3 10 20 10)))
    -- cube below
    queueObject queue (makeLine (V3 (-10) (-10) (-10))    (V3 10  (-10)  (-10)))
    queueObject queue (makeLine (V3 10  (-10)  (-10)) (V3 10 10  (-10)))
    queueObject queue (makeLine (V3 10 10  (-10)) (V3  (-10) 10  (-10)))
    queueObject queue (makeLine (V3  (-10) 10  (-10)) (V3  (-10)  (-10) (-10)))
    queueObject queue (makeLine (V3  (-10)  (-10) (-10)) (V3  (-10) (-10) 10))
    queueObject queue (makeLine (V3 10 (-10) (-10))  (V3 10 (-10) 10))
    queueObject queue (makeLine (V3 10 10 (-10))  (V3 10 10 10))
    queueObject queue (makeLine (V3  (-10) 10 (-10))  (V3  (-10) 10 10))
    queueObject queue (makeLine (V3  (-10)  (-10) 10) (V3 10  (-10) 10))
    queueObject queue (makeLine (V3 10 (-10) 10) (V3 10 10 10))
    queueObject queue (makeLine (V3 10 10 10) (V3  (-10) 10 10))
    queueObject queue (makeLine (V3 (-10) 10 10) (V3  (-10) (-10) 10))
