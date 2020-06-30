module GLFW_Helpers
( 
  glfwInit
, initFailMsg
)where

-- base
import Control.Monad (when)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)

-- third party
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33

-- internal
import ViewSpace (Camera, rotateCameraNudge, zoomCamera)

-- | This data is used to determine how far the cursor has moved
data CursorPosition = CursorPosition Float Float

-- | Initializes a GLFW window, including the openGL context
glfwInit :: Camera -> Int -> Int -> String -> IO (Maybe GLFW.Window)
glfwInit camera width height title = do
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
    GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    GLFW.windowHint (GLFW.WindowHint'Resizable True)
    GLFW.init
    maybeWindow <- GLFW.createWindow width height title Nothing Nothing
    case maybeWindow of
        Nothing -> GLFW.terminate >> pure Nothing
        Just window -> fmap Just (initWindow window camera)

initWindow :: GLFW.Window -> Camera -> IO GLFW.Window
initWindow window ioCam = do
    -- Initialise (global... :(  ) cursor info
    cursor <- newIORef $ CursorPosition 0 0

    -- enable callbacks
    GLFW.setKeyCallback window (Just (keypressed ioCam))
    GLFW.setFramebufferSizeCallback window ( Just resize )
    GLFW.setMouseButtonCallback window (Just (mouseButtonPressed ioCam cursor))
    GLFW.setScrollCallback window ( Just (mouseScrolled ioCam) )

    -- calibrate the viewport
    GLFW.makeContextCurrent (Just window)
    (x,y) <- GLFW.getFramebufferSize window
    glViewport 0 0 (fromIntegral x) (fromIntegral y)

    -- enable depth testing
    glEnable GL_DEPTH_TEST

    pure window

initFailMsg :: IO ()
initFailMsg = do
    putStrLn "Failed to create a GLFW window!"
    putStrLn "  are you sure glfw is installed?"
    putStrLn "  If you're using Intel, you may need to enable software rendering"
    putStrLn "  If you're using a terminal, you may need to set DISPLAY."

-- ===========================================================================
--                            Callbacks
-- ===========================================================================
-- | callback for when the user presses a key
-- type KeyCallback = Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
keypressed :: Camera -> GLFW.KeyCallback
keypressed cam window key scanCode keyState modKeys = do
    let delta = 0.1
    when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
        (GLFW.setWindowShouldClose window True)
    --when (key == GLFW.Key'C && keyState == GLFW.KeyState'Pressed)
        --(rotateCameraNudge cam 0 delta)
        --(setCamera cam (V3 0 0 50))
    when (key == GLFW.Key'Up && (elem keyState [GLFW.KeyState'Pressed, GLFW.KeyState'Repeating]))
        (rotateCameraNudge cam 0 delta)
    when (key == GLFW.Key'Down && (elem keyState [GLFW.KeyState'Pressed, GLFW.KeyState'Repeating]))
        (rotateCameraNudge cam 0 (-delta))
    when (key == GLFW.Key'Right && (elem keyState [GLFW.KeyState'Pressed, GLFW.KeyState'Repeating]))
        (rotateCameraNudge cam (-delta) 0)
    when (key == GLFW.Key'Left && (elem keyState [GLFW.KeyState'Pressed, GLFW.KeyState'Repeating]))
        (rotateCameraNudge cam (delta) 0)

-- | callback for when the user resizes the window
resize :: GLFW.FramebufferSizeCallback
resize _ width height = do
    glViewport 0 0 (fromIntegral width) (fromIntegral height)

-- | callback for when the cursor is moved inside the window
-- GLFW.CursorPosCallback :: GLFW.Window -> Double -> Double -> IO ()
cursorMoved :: IORef CursorPosition -> Camera -> GLFW.CursorPosCallback
cursorMoved ioCursor camera _ x y = do
    -- Calculate delta
    (CursorPosition x0 y0) <- readIORef ioCursor
    let sensitivity = 0.1
        x' = realToFrac x
        y' = realToFrac y
        dx = sensitivity * (x' - x0)
        dy = -1 * sensitivity * (y' - y0)

    -- Update our IORef (err....global var.)
    writeIORef  ioCursor (CursorPosition x' y')

    -- Update camera
    rotateCameraNudge camera (-dx) dy

-- | Callback for when the user presses a button in the window
mouseButtonPressed :: Camera -> IORef CursorPosition -> GLFW.MouseButtonCallback
mouseButtonPressed cam cursor window GLFW.MouseButton'1 state _ = do
    if state == GLFW.MouseButtonState'Pressed
       then do
           (x, y) <- GLFW.getCursorPos window
           writeIORef cursor (CursorPosition (realToFrac x) (realToFrac y))
           GLFW.setCursorPosCallback window (Just (cursorMoved cursor cam))
           GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled
       else do
           GLFW.setCursorPosCallback window Nothing
           GLFW.setCursorInputMode window GLFW.CursorInputMode'Normal
           -- re-enable the cursor
mouseButtonPressed _ _ _ _ _ _ = pure ()

-- | Callback for when the user scrolls the mouse wheel
mouseScrolled :: Camera -> GLFW.ScrollCallback
mouseScrolled camera _ _ dy = do
    zoomCamera camera (realToFrac dy)

