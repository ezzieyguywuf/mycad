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
    when (key == GLFW.Key'Right && keyState == GLFW.KeyState'Pressed)
        (moveCamera cam delta 0)
    when (key == GLFW.Key'Left && keyState == GLFW.KeyState'Pressed)
        (moveCamera cam (-delta) 0)

resize :: GLFW.FramebufferSizeCallback
resize _ width height = do
    glViewport 0 0 (fromIntegral width) (fromIntegral height)

updateCursor :: CursorPosition -> Float -> Float -> CursorPosition
updateCursor cursor x y = CursorPosition x y dx' dy' cumDX cumDY
    where sensitivity = 600
          x0 = getLastX cursor
          y0 = getLastY cursor
          dx = getDeltaX cursor + (x0 - x)
          dy = getDeltaY cursor + (y0 - y)
          (dx', cumDX) = case compare dx sensitivity of
                          GT -> (dx, 0)
                          _  -> (0, dx)
          (dy', cumDY) = case compare dy sensitivity of
                          GT -> (dy, 0)
                          _  -> (0, dy)

-- GLFW.CursorPosCallback :: GLFW.Window -> Double -> Double -> IO ()
cursorMoved :: IORef CursorPosition -> IORef Camera -> GLFW.CursorPosCallback
cursorMoved ioCursor camera _ x y = do
    -- Calculate delta
    cursor <- readIORef ioCursor
    let cursor' = updateCursor cursor (realToFrac x) (realToFrac y)

    -- Update our IORef (err....global var.)
    writeIORef  ioCursor cursor'

    putStrLn $ "x = " <> (show x) <> ", y = " <> (show y)
    putStrLn $ "dx = " <> (show $ getDeltaX cursor') <> ", dy = " <> (show $ getDeltaY cursor')
    putStrLn $ "cumdx = " <> (show $ _cumDeltaX cursor') <> ", cumdy = " <> (show $ _cumDeltaY cursor')

    -- Update camera
    moveCamera camera (getDeltaX cursor') (getDeltaY cursor')

data CursorPosition = CursorPosition
    {
      getLastX :: Float
    , getLastY :: Float
    , getDeltaX :: Float
    , getDeltaY :: Float
    , _cumDeltaX :: Float
    , _cumDeltaY :: Float
    }

mouseButtonPressed :: IORef Camera -> IORef CursorPosition -> GLFW.MouseButtonCallback
mouseButtonPressed cam cursor window GLFW.MouseButton'1 state _ = do
    if state == GLFW.MouseButtonState'Pressed
       then do
           GLFW.setCursorPosCallback window (Just (cursorMoved cursor cam))
           GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled
       else do
           GLFW.setCursorPosCallback window Nothing
           GLFW.setCursorInputMode window GLFW.CursorInputMode'Normal
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
    cursor <- newIORef $ CursorPosition 0 0 0 0 0 0

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

