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

-- type KeyCallback = Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
keypressed :: GLFW.KeyCallback
keypressed window key scanCode keyState modKeys = do
    print key
    when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
        (GLFW.setWindowShouldClose window True)

resize :: GLFW.FramebufferSizeCallback
resize _ width height = do
    glViewport 0 0 (fromIntegral width) (fromIntegral height)

cursorPosition :: GLFW.CursorPosCallback
cursorPosition _ x y = do
    putStrLn $ "x = " <> (show x) <> ", y = " <> (show y)


mouseButtonPressed :: GLFW.MouseButtonCallback
mouseButtonPressed window GLFW.MouseButton'1 state _ =
    if state == GLFW.MouseButtonState'Pressed
       then do
          GLFW.setCursorPosCallback window (Just cursorPosition)
          GLFW.setCursorInputMode window GLFW.CursorInputMode'Hidden
        else do
          GLFW.setCursorPosCallback window Nothing
          GLFW.setCursorInputMode window GLFW.CursorInputMode'Normal
mouseButtonPressed _ _ _ _ = pure ()

glfwInit :: Int -> Int -> String -> IO (Maybe GLFW.Window)
glfwInit width height title = do
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
    GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    GLFW.windowHint (GLFW.WindowHint'Resizable True)
    GLFW.createWindow width height title Nothing Nothing

glfwWindowInit :: GLFW.Window -> IO ()
glfwWindowInit window = do
    -- enable callbacks
    GLFW.setKeyCallback window (Just keypressed )
    GLFW.setFramebufferSizeCallback window ( Just resize )
    GLFW.setMouseButtonCallback window (Just mouseButtonPressed)

    -- calibrate the viewport
    GLFW.makeContextCurrent (Just window)
    (x,y) <- GLFW.getFramebufferSize window
    glViewport 0 0 (fromIntegral x) (fromIntegral y)

initFailMsg :: IO ()
initFailMsg = do
    putStrLn "Failed to create a GLFW window!"
    putStrLn "  are you sure glfw is installed?"
    putStrLn "  if you're using Intel, you may need to enable software rendering"

