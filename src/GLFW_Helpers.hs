module GLFW_Helpers
( keypressed
, resize
, glfwInit
, glfwWindowInit
, initFailMsg
)where

-- base
import Control.Monad (when)

-- GLFW-b, qualified for clarity
import qualified Graphics.UI.GLFW as GLFW

-- gl, all types and funcs here will already start with "gl"
import Graphics.GL.Core33

import Data.IORef
import ViewSpace

-- type KeyCallback = Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
keypressed :: IORef Camera -> GLFW.KeyCallback
keypressed cam window key scanCode keyState modKeys = do
    let delta = 10 * pi / 180.0
    when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
        (GLFW.setWindowShouldClose window True)
    when (key == GLFW.Key'Up && keyState == GLFW.KeyState'Pressed)
        (moveCamera cam 0 delta)
    when (key == GLFW.Key'Down && keyState == GLFW.KeyState'Pressed)
        (moveCamera cam 0 (-delta))

resize :: GLFW.FramebufferSizeCallback
resize _ width height = do
    glViewport 0 0 (fromIntegral width) (fromIntegral height)

-- GLFW.CursorPosCallback :: GLFW.Window -> Double -> Double -> IO ()
cursorMoved :: IORef CursorPosition -> IORef Camera -> GLFW.CursorPosCallback
cursorMoved ioCursor camera _ yaw pitch = do
    -- Calculate delta
    cursor <- readIORef ioCursor
    let sensitivity = 0.01
        yaw0 = getLastX cursor
        deltaYaw = -1 * sensitivity * ((realToFrac yaw) - yaw0)
        pitch0 = getLastY cursor
        deltaPitch = sensitivity * ((realToFrac pitch) - pitch0)
    let updatedCursor = CursorPosition orig (realToFrac yaw) (realToFrac pitch)
        orig = getOriginalPosition cursor
    writeIORef ioCursor updatedCursor

    -- Update camera
    moveCamera camera deltaYaw deltaPitch

data CursorPosition = CursorPosition
    { getOriginalPosition :: (Double, Double)
    , getLastX :: Float
    , getLastY :: Float
    }

initializeCursor :: GLFW.Window -> IORef CursorPosition -> IO ()
initializeCursor window ioCursor = do
    (x, y) <- GLFW.getCursorPos window
    let x' = realToFrac x
        y' = realToFrac y
    writeIORef ioCursor $ CursorPosition (x, y) x' y'

mouseButtonPressed :: IORef Camera -> IORef CursorPosition -> GLFW.MouseButtonCallback
mouseButtonPressed cam cursor window GLFW.MouseButton'1 state _ = do
    if state == GLFW.MouseButtonState'Pressed
       then do

           initializeCursor window cursor
           GLFW.setCursorPosCallback window (Just (cursorMoved cursor cam))
           GLFW.setCursorInputMode window GLFW.CursorInputMode'Hidden
       else do
           cursor' <- readIORef cursor
           let (x, y) = getOriginalPosition cursor'

           GLFW.setCursorPosCallback window Nothing
           -- Disable first, for the Wayland folks
           GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled
           GLFW.setCursorPos window x y
           -- Then also do it this way, for good measure
           GLFW.setCursorInputMode window GLFW.CursorInputMode'Normal
           GLFW.setCursorPos window x y
           -- re-enable the cursor
mouseButtonPressed _ _ _ _ _ _ = pure ()

glfwInit :: Int -> Int -> String -> IO (Maybe GLFW.Window)
glfwInit width height title = do
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
    GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    GLFW.windowHint (GLFW.WindowHint'Resizable True)
    GLFW.createWindow width height title Nothing Nothing

glfwWindowInit :: GLFW.Window -> IORef Camera -> IO ()
glfwWindowInit window ioCam = do
    -- Initialise (global... :(  ) cursor info
    (x, y) <- GLFW.getCursorPos window
    cursor <- newIORef $ CursorPosition (x, y) 0 0

    -- enable callbacks
    GLFW.setKeyCallback window (Just (keypressed ioCam))
    GLFW.setFramebufferSizeCallback window ( Just resize )
    GLFW.setMouseButtonCallback window (Just (mouseButtonPressed ioCam cursor))

    -- calibrate the viewport
    GLFW.makeContextCurrent (Just window)
    (x,y) <- GLFW.getFramebufferSize window
    glViewport 0 0 (fromIntegral x) (fromIntegral y)

initFailMsg :: IO ()
initFailMsg = do
    putStrLn "Failed to create a GLFW window!"
    putStrLn "  are you sure glfw is installed?"
    putStrLn "  if you're using Intel, you may need to enable software rendering"

