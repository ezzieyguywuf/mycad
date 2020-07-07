module GLFW_Helpers
(
  Window(..)
, shouldClose
, shutdownGLFW
, swapBuffers
, glfwInit
, releaseContext
, takeContext
)where

-- base
import Control.Monad (when)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue, newTQueue, writeTQueue)

-- third party
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33

-- internal
import ViewSpace (CameraData, rotateCameraNudge, zoomCamera)

-- | A Window includes the data needed to communicate with GLFW, as well as
--   information about the View
data Window = Window { getWindow   :: GLFW.Window
                     , lastCamera  :: IORef CameraData
                     , cameraQueue :: TQueue CameraData
                     }

-- | This data is used to determine how far the cursor has moved in callbacks
data CursorPosition = CursorPosition Float Float

-- | Initializes a GLFW window, including the openGL context
glfwInit :: Int -> Int -> String -> CameraData -> IO (Maybe Window)
glfwInit width height title camera = do
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
    GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    GLFW.windowHint (GLFW.WindowHint'Resizable True)
    GLFW.init
    maybeWindow <- GLFW.createWindow width height title Nothing Nothing
    maybe (shutdownGLFW >> pure Nothing) (initWindow camera) maybeWindow

initWindow :: CameraData -> GLFW.Window -> IO (Maybe Window)
initWindow camera glfwWindow = do
    -- Initialize...well, global stuf :(
    ioCam  <- newIORef camera
    cursor <- newIORef $ CursorPosition 0 0
    camQueue <- atomically newTQueue :: IO (TQueue CameraData)
    let window = Window glfwWindow ioCam camQueue

    -- This sets the initial camera view.
    atomically $ writeTQueue camQueue camera

    -- enable callbacks
    GLFW.setKeyCallback glfwWindow (Just (keypressed window))
    GLFW.setFramebufferSizeCallback glfwWindow ( Just (resize window))
    GLFW.setWindowRefreshCallback glfwWindow ( Just (refresh window) )
    GLFW.setMouseButtonCallback glfwWindow (Just (mouseButtonPressed window cursor))
    GLFW.setScrollCallback glfwWindow ( Just (mouseScrolled window) )

    -- calibrate the viewport
    GLFW.makeContextCurrent (Just glfwWindow)
    (x,y) <- GLFW.getFramebufferSize glfwWindow
    glViewport 0 0 (fromIntegral x) (fromIntegral y)

    pure $ Just window

-- | Determine if the User or OS has requested for the window to close.
shouldClose :: Window -> IO Bool
shouldClose window = GLFW.windowShouldClose (getWindow window)

-- | Exit the GLFW stuff
shutdownGLFW :: IO ()
shutdownGLFW = GLFW.terminate

swapBuffers :: Window -> IO ()
swapBuffers window = GLFW.swapBuffers (getWindow window)

-- | Releases the OpenGL \"Context\" from the current thread
releaseContext :: IO ()
releaseContext = GLFW.makeContextCurrent Nothing

-- | Sets the OpenGL \"Context\" in the specified "Window" to the current thread.
takeContext :: Window -> IO ()
takeContext window = GLFW.makeContextCurrent(Just $ getWindow window)

-- ===========================================================================
--                            Callbacks
-- ===========================================================================
-- | callback for when the user presses a key
-- type KeyCallback = Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
keypressed :: Window -> GLFW.KeyCallback
keypressed window glfwWindow key _ keyState _ = do
    let delta = 0.1
        isPressed   = keyState == GLFW.KeyState'Pressed
        isRepeating = keyState == GLFW.KeyState'Repeating
        isEscape    = key == GLFW.Key'Escape
        isUp        = key == GLFW.Key'Up
        isDown      = key == GLFW.Key'Down
        isLeft      = key == GLFW.Key'Left
        isRight     = key == GLFW.Key'Right

    when (isPressed && isEscape) (GLFW.setWindowShouldClose glfwWindow True)
    when (isUp    && (isPressed || isRepeating)) (bumpCamera window 0   delta)
    when (isDown  && (isPressed || isRepeating)) (bumpCamera window 0 (-delta))
    when (isRight && (isPressed || isRepeating)) (bumpCamera window (-delta) 0)
    when (isLeft  && (isPressed || isRepeating)) (bumpCamera window   delta  0)

bumpCamera :: Window -> Float -> Float -> IO ()
bumpCamera window dx dy = do
    let ioCam    = lastCamera window
        camQueue = cameraQueue window
    -- Get the previous camera information
    oldCamData <- readIORef ioCam

    -- Transform the camera to it's new position
    let newCamData = rotateCameraNudge oldCamData dx dy

    -- Update our lastCamera IORef
    writeIORef ioCam newCamData
    -- Let whoever's listening know that there's new data to use.
    atomically $ writeTQueue camQueue newCamData


-- | callback for when the user resizes the window
resize :: Window -> GLFW.FramebufferSizeCallback
resize window _ width height = do
    glViewport 0 0 (fromIntegral width) (fromIntegral height)
    sendRedraw window

-- | callback when the window has been damaged and needs to be refreshed
refresh :: Window -> GLFW.WindowRefreshCallback
refresh window _ = sendRedraw window

-- | Helper - puts the current camera view back into the Queue, in order to trigger a redraw
sendRedraw :: Window -> IO ()
sendRedraw window = do
    let ioCam    = lastCamera window
        camQueue = cameraQueue window

    -- Get the camera information
    camData <- readIORef ioCam

    -- Put it in the Queue, which should trigger a redraw in main.
    atomically $ writeTQueue camQueue camData

-- | callback for when the cursor is moved inside the window
-- GLFW.CursorPosCallback :: GLFW.Window -> Double -> Double -> IO ()
cursorMoved :: IORef CursorPosition -> Window -> GLFW.CursorPosCallback
cursorMoved ioCursor window _ x y = do
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
    bumpCamera window (-dx) dy

-- | Callback for when the user presses a button in the window
mouseButtonPressed :: Window -> IORef CursorPosition -> GLFW.MouseButtonCallback
mouseButtonPressed window cursor glfwWindow button state _ = do
    let isPressed = state  == GLFW.MouseButtonState'Pressed
        isMB1     = button == GLFW.MouseButton'1
    if isMB1 && isPressed
       then do
           -- track the cursor's movement
           (x, y) <- GLFW.getCursorPos glfwWindow
           writeIORef cursor (CursorPosition (realToFrac x) (realToFrac y))
           GLFW.setCursorPosCallback glfwWindow (Just (cursorMoved cursor window))
           GLFW.setCursorInputMode glfwWindow GLFW.CursorInputMode'Disabled
       else do
           -- stop tracking the cursor's movement
           GLFW.setCursorPosCallback glfwWindow Nothing
           GLFW.setCursorInputMode glfwWindow GLFW.CursorInputMode'Normal

-- | Callback for when the user scrolls the mouse wheel
mouseScrolled :: Window -> GLFW.ScrollCallback
mouseScrolled window _ _ dy = do
    oldCamData <- readIORef (lastCamera window)
    let newCamData = zoomCamera oldCamData (realToFrac dy)
    atomically $ writeTQueue (cameraQueue window) newCamData
