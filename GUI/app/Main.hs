-- base
import Control.Monad (when)
import Control.Exception (bracket)
import Data.Bits

-- GLFW-b, qualified for clarity
import qualified Graphics.UI.GLFW as GLFW

-- helpers that we wrote
import GLFW_Helpers
import GL_Helpers
import VertexData
import ViewSpace

-- gl, all types and funcs here will already start with "gl"
import Graphics.GL.Core33

-- For Linear algebra...but really, like vectors and matrices and quaternions
import Linear.V3
import Linear.Metric
import Linear.Projection
import Linear.Quaternion

import Data.IORef
import System.Console.ANSI
import GraphicData

main :: IO ()
main = bracket GLFW.init (const GLFW.terminate) $ \initWorked ->
    when initWorked act

winWIDTH = 800
winHEIGHT = 600
winASPECT = (fromIntegral winWIDTH) / (fromIntegral winHEIGHT)
winTITLE = "LearnOpenGL Hello Line!"

act :: IO()
act = do
    maybeWindow <- glfwInit winWIDTH winHEIGHT winTITLE
    case maybeWindow of
        Nothing -> initFailMsg
        Just window -> do
            -- Initialize console output
            initializeConsole

            -- Set up some...well global variables
            camera <- initCamera

            -- Initialize glfw things, including callbacks
            glfwWindowInit window camera

            -- Compile and like our shaders
            baseShader <- makeShader "./src/VertexShader.glsl" "./src/FragmentShader.glsl"
            lineShader <- makeShader "./src/LineVShader.glsl" "./src/FragmentShader.glsl"

            cubeDrawer <- makeObjectDrawer baseShader cube
            lineDrawer' <- makeObjectDrawer baseShader line'
            lineDrawer <- makeObjectDrawer lineShader lineElement'
            --circleDrawer <- makeObjectDrawer lineShader circle
            --lineCubeDrawer <- makeObjectDrawer lineShader wireCube

            -- enable depth testing
            glEnable GL_DEPTH_TEST

            -- set static uniforms
            putProjectionUniform baseShader
            putProjectionUniform lineShader

            floatUniform winASPECT "aspect" >>= putUniform lineShader

            ioTick <- newIORef 0 :: IO (IORef Float)
            ioLines <- newIORef ("", "", "", "", "") 
            -- enter our main loop
            let loop = do
                    shouldContinue <- not <$> GLFW.windowShouldClose window
                    when shouldContinue $ do
                        -- event poll
                        GLFW.pollEvents
                        -- drawing
                        --   Background
                        glClearColor 0.2 0.3 0.3 1.0
                        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

                        -- draw static objects
                        drawObject cubeDrawer
                        drawObject lineDrawer'
                        drawObject lineDrawer

                        (LookAt loc _ dir) <- readIORef camera
                        let vect = normalize (loc - dir)
                            theta = cos (vect `dot` (V3 0 0 1))
                            target = rotateElement line'' (axisAngle (V3 0 1 0) theta)
                        makeObjectDrawer lineShader target >>= drawObject

                        --drawObject circleDrawer

                        -- Update our uniforms
                        putViewUniform camera baseShader
                        putViewUniform camera lineShader

                        -- Draw the rotating line
                        time <- maybe 0 realToFrac <$> GLFW.getTime
                        --let p0 = V3 (-15) 15 0
                            --p2 = V3 (-15) (-15) 0
                            --axis = axisAngle (V3 0 0 1) (pi * ((sin time) + 1))
                            --p1' = Linear.Quaternion.rotate (axis) (p0 - p2)
                            --p1 = p1' + p2
                            --width = 3.0
                        --cam <- readIORef camera
                        --lineDrawer2 <- makeObjectDrawer lineShader (makeLine width p1 p2)

                        --drawObject lineDrawer2
                        --drawObject lineCubeDrawer

                        --rotateCameraNudge camera (-0.005) 0

                        tick <- readIORef ioTick
                        let tick' = time + tick
                            tick'' = case compare tick' 100 of
                                        LT -> tick'
                                        _  -> 0
                        writeIORef ioTick tick''

                        when (tick'' == 0) (do
                            (l1, l2, l3, l4, _) <- readIORef ioLines
                            (LookAt loc _ dir) <- readIORef camera
                            --let lineNormal = V3 0 0 1 :: V3 Float
                                --view = loc - dir
                            let lines' = (("tick " <> (show time)), l1, l2, l3, l4)
                            putConsole lines'
                            writeIORef ioLines lines')
                        -- swap buffers and go again
                        GLFW.swapBuffers window
                        loop
            loop
    GLFW.terminate

putViewUniform :: IORef Camera -> Shader -> IO ()
putViewUniform ioCam shader = do
    (LookAt loc up dir) <- readIORef ioCam
    matrixUniform (lookAt loc dir up) "view" >>= (putUniform shader)


putProjectionUniform :: Shader -> IO ()
putProjectionUniform shader = matrixUniform projectionMatrix "projection" >>= (putUniform shader)
    where projectionMatrix = perspective (pi/4.0) winASPECT 0.1 1000.0

initCamera :: IO (IORef Camera)
initCamera = newIORef LookAt { 
                               location  = V3 0 0 100
                             , up        = V3 0 1 0
                             , direction = V3 0 0 0
                             }

initializeConsole :: IO ()
initializeConsole = do
    putStrLn "All data should update only below here. Welcome!"
    sequence_ $ take 5 (repeat $ putStrLn "")

putConsole :: (String, String, String, String, String) -> IO()
putConsole (l1, l2, l3, l4, l5) = do
    cursorUp 5
    sequence_ (map putStrLn ([l1, l2, l3, l4, l5]))
