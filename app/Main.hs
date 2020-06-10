-- base
import Control.Monad (when)
import Control.Exception (bracket)
-- GLFW-b, qualified for clarity
import qualified Graphics.UI.GLFW as GLFW
-- gl, all types and funcs here will already start with "gl"
import Graphics.GL.Core33
import Graphics.GL.Types

-- This is for the Foreign Function Interface, ffi. This calls C-code
import Foreign
-- Converts Haskell strings to C-strings
import Foreign.C.String (withCAStringLen, newCString)

-- For loading images
import Codec.Picture ( readImage
                     , generateImage
                     , convertRGB8
                     , DynamicImage(..)
                     , Image(..)
                     , PixelRGB8(..))

-- For doing silly things with vector pointers
import qualified Data.Vector.Storable as VS

-- For Linear algebra...but really, like vectors and matrices and quaternions
{-import Linear.V3-}
{-import Linear.V4-}
{-import Linear.Quaternion-}
{-import Linear.Quaternion-}


winWidth = 800

winHeight = 600

winTitle = "LearnOpenGL Hello Triangle"

-- type KeyCallback = Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
keypressed :: GLFW.KeyCallback
keypressed window key scanCode keyState modKeys = do
    print key
    when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
        (GLFW.setWindowShouldClose window True)

resize :: GLFW.FramebufferSizeCallback
resize _ width height = do
    glViewport 0 0 (fromIntegral width) (fromIntegral height)

main :: IO ()
main = bracket GLFW.init (const GLFW.terminate) $ \initWorked ->
    when initWorked act

act :: IO()
act = do
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
    GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    GLFW.windowHint (GLFW.WindowHint'Resizable True)
    maybeWindow <- GLFW.createWindow winWidth winHeight winTitle Nothing Nothing
    case maybeWindow of
        Nothing -> putStrLn "Failed to create a GLFW window!" >>
                   putStrLn "  are you sure glfw is installed?" >>
                   putStrLn "  if you're using Intel, you may need to enable software rendering"
        Just window -> do
            -- enable keys
            GLFW.setKeyCallback window (Just keypressed )
            GLFW.setFramebufferSizeCallback window ( Just resize )
            -- calibrate the viewport
            GLFW.makeContextCurrent (Just window)
            (x,y) <- GLFW.getFramebufferSize window
            glViewport 0 0 (fromIntegral x) (fromIntegral y)

            vs <- vertexShaderSource >>= loadShader GL_VERTEX_SHADER
            fs <- fragmentShaderSource >>= loadShader GL_FRAGMENT_SHADER

            shaderProgram <- linkShadersToProgram vs fs

            -- I guess these aren't needed any more?
            glDeleteShader vs
            glDeleteShader fs

            --          positions           colors    Texture Coords
            let vs = [  0.5,  0.5, 0.0, 1.0, 0.0, 0.0, 1.0, 1.0  -- top right
                     ,  0.5, -0.5, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0  -- bottom right
                     , -0.5, -0.5, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0  -- bottom left
                     , -0.5,  0.5, 0.0, 1.0, 1.0, 1.0, 0.0, 1.0  -- top left
                     ] :: [GLfloat]

            let inds  = [ 0, 1, 2
                        , 2, 3, 0] :: [GLuint]

            vao <- makeVertices vs inds

            -- Wireframe mode
            {-glPolygonMode GL_FRONT_AND_BACK GL_LINE-}

            -- An openGL-compatible string containing the name of our Uniforms
            ourColor <- newCString "ourColor"
            texture0 <- newCString "texture0"
            texture1 <- newCString "texture1"

            -- Load the texture information into opengl
            texture0_id <- loadTexture "res/container.jpg"
            texture1_id <- loadTexture "res/awesomeface.png"

            -- Find out where in the program the uniforms are located
            loc0 <- glGetUniformLocation shaderProgram texture0
            loc1 <- glGetUniformLocation shaderProgram texture1

            -- We must call UseProgram before glUniformX, otherwise it won't
            -- know which program to put it in!
            glUseProgram shaderProgram

            -- Bind the texture locations to the indices we want them
            -- associated with in the fragment shader
            glUniform1i loc0 0
            glUniform1i loc1 1
            -- enter our main loop
            let loop = do
                    shouldContinue <- not <$> GLFW.windowShouldClose window
                    when shouldContinue $ do
                        -- event poll
                        GLFW.pollEvents
                        -- drawing
                        --   Background
                        glClearColor 0.2 0.3 0.3 1.0
                        glClear GL_COLOR_BUFFER_BIT

                        -- Use our program
                        glUseProgram shaderProgram

                        -- Bind the texture we want to use
                        -- Tell openGL about our Uniforms
                        glActiveTexture GL_TEXTURE0
                        glBindTexture GL_TEXTURE_2D texture0_id

                        glActiveTexture GL_TEXTURE1
                        glBindTexture GL_TEXTURE_2D texture1_id

                        -- draw the triangle.
                        glBindVertexArray vao
                        glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT nullPtr
                        glBindVertexArray 0
                        -- swap buffers and go again
                        GLFW.swapBuffers window
                        loop
            loop
    GLFW.terminate

getColor :: IO GLfloat
getColor = do
    timeValue <- maybe 0 realToFrac <$> GLFW.getTime
    pure $ sin timeValue / 2 + 0.5

makeVertices:: [GLfloat] -> [GLuint] -> IO GLuint
makeVertices vertices indices = do
    let verticesSize = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length vertices)

    --   This converts our vertices into something that openGL understands - I
    --   think it's a pointer, thus the P
    verticesP <- newArray vertices

    -- This creates 1 buffer in openGL In C, it would be:
    --
    --     unsigned int VBO;
    --     glGenBuffers(1, &VBO);
    --
    --  Why is it longer is Haskell? Because we are being more explicit about
    --  what we're doing.
    vboP <- malloc       -- Give me a pointer to 'someting'
    glGenBuffers 1 vboP  -- Make the pointer (this determines what 'something'
                         -- is. It's 'Int' in the c-code)
    vbo <- peek vboP     -- vbo is the uniqueID that openGL generated, unsigned
                         -- int VBO in c

    -- Now we can "bind" the buffer. This allows us to write to it. In C, this
    -- was:
    --
    --     glBindBuffer(GL_ARRAY_BUFFER, VBO);
    --     glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);
    glBindBuffer GL_ARRAY_BUFFER vbo
    glBufferData GL_ARRAY_BUFFER verticesSize (castPtr verticesP) GL_STATIC_DRAW

    -- The Vertex Array Object, or VAO, allows us to store a Vertex Buffer
    -- Object and attribute data to re-use later. I guess it's like an object?
    vaoP <- malloc
    glGenVertexArrays 1 vaoP
    vao <- peek vaoP

    -- ...:: initializing code, done once unless your object frequently changes
    -- 1. bind Vertex Array Object
    glBindVertexArray vao
    -- 2. copy our vertices array in a buffer for openGL to use
    glBindBuffer GL_ARRAY_BUFFER vbo
    glBufferData GL_ARRAY_BUFFER verticesSize (castPtr verticesP) GL_STATIC_DRAW
    -- 3. Next, set our vertex attribute pointers
    let floatSize = fromIntegral $ sizeOf (0.0::GLfloat) :: GLsizei
        nData     = 8

    -- position attribute
    glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (nData * floatSize) nullPtr
    glEnableVertexAttribArray 0

    -- color attribute
    let offset = castPtr $ plusPtr nullPtr (fromIntegral $ 3*floatSize)
    glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE (nData * floatSize) offset
    glEnableVertexAttribArray 1

    -- texture attribute
    let sixFloatOffset = castPtr $ plusPtr nullPtr (fromIntegral $ 6*floatSize)
    glVertexAttribPointer 2 2 GL_FLOAT GL_FALSE (nData * floatSize) sixFloatOffset
    glEnableVertexAttribArray 2


    -- Prep the indices for use in the EBO
    let indicesSize = fromIntegral $ sizeOf (0 :: GLuint) * (length indices)
    indicesP <- newArray indices

    -- The Element Buffer Object, or EBO, allows us to re-use vertices in the Buffer. this
    -- let's us save space on the graphics memory.
    -- We sould do this after the VAO has been bound, because then ith VAO will
    -- automatically store a reference to this EBO
    eboP <- malloc
    glGenBuffers 1 eboP
    ebo <- peek eboP
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
    glBufferData GL_ELEMENT_ARRAY_BUFFER indicesSize (castPtr indicesP) GL_STATIC_DRAW


    -- unbind the array when we're  done, I guess?
    glBindVertexArray 0

    -- 4. (in the loop) draw the object (see :main)
    pure vao

vertexShaderSource :: IO String
vertexShaderSource = readFile "./src/VertexShader.glsl"

fragmentShaderSource :: IO String
fragmentShaderSource = readFile "./src/FragmentShader.glsl"

linkShadersToProgram :: GLuint -> GLuint -> IO GLuint
linkShadersToProgram shader1 shader2 = do
    shaderProgram <- glCreateProgram

    glAttachShader shaderProgram shader1
    glAttachShader shaderProgram shader2
    glLinkProgram shaderProgram

    linkingSuccessP <- malloc
    glGetProgramiv shaderProgram GL_LINK_STATUS linkingSuccessP
    linkingSuccess <- peek linkingSuccessP
    when (linkingSuccess == GL_FALSE) $ do
        putStrLn "Program Linking Error:"
        let infoLength = 512
        resultP <- malloc
        infoLog <- mallocArray (fromIntegral infoLength)
        glGetProgramInfoLog shaderProgram (fromIntegral infoLength) resultP infoLog
        result <- fromIntegral <$> peek resultP
        logBytes <- peekArray result infoLog
        putStrLn (map (toEnum.fromEnum) logBytes)

    pure shaderProgram

-- | Given a shader type and a shader source, it gives you (Right id) of the
-- successfully compiled shader, or (Left err) with the error message. In the
-- error case, the shader id is deleted before the function returns to avoid
-- accidentally leaking shader objects.
loadShader :: GLenum -> String -> IO GLuint
loadShader shaderType source = do
    -- new shader object
    shaderID <- glCreateShader shaderType

    -- assign the source to the shader object
    withCAStringLen source $ \(strP, strLen) ->
        withArray [strP] $ \linesPtrsPtr ->
            withArray [fromIntegral strLen] $ \lengthsPtr ->
                glShaderSource shaderID 1 linesPtrsPtr lengthsPtr

    -- compile and check success
    glCompileShader shaderID
    success <- alloca $ \successP -> do
        glGetShaderiv shaderID GL_COMPILE_STATUS successP
        peek successP
    if success == GL_TRUE
        -- success: we're done
        then pure shaderID
        -- failure: we get the log, delete the shader, and return the log.
        else do
            -- how many bytes the info log should be (including the '\0')
            logLen <- alloca $ \logLenP -> do
                glGetShaderiv shaderID GL_INFO_LOG_LENGTH logLenP
                peek logLenP
            -- space for the info log
            logBytes <- allocaBytes (fromIntegral logLen) $ \logP -> do
                -- space for the log reading result
                alloca $ \resultP -> do
                    -- Try to obtain the log bytes
                    glGetShaderInfoLog shaderID logLen resultP logP
                    -- this is how many bytes we actually got
                    result <- fromIntegral <$> peek resultP
                    peekArray result logP
            -- delete the shader object and return the log
            glDeleteShader shaderID
            let prefix = case shaderType of
                    GL_VERTEX_SHADER -> "Vertex"
                    GL_GEOMETRY_SHADER -> "Geometry"
                    GL_FRAGMENT_SHADER -> "Fragment"
                    _ -> "Unknown Type"
            putStrLn $ prefix ++ " Shader Error:" ++
                        (map (toEnum.fromEnum) logBytes)
            pure 0

generateGradient :: DynamicImage
generateGradient = ImageRGB8 $ generateImage renderer 800 600
    where renderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128

getDynImage :: String -> IO DynamicImage
getDynImage fname = do
    check <- readImage fname
    case check of
        Left msg -> pure generateGradient
        Right rawData -> pure rawData

loadTexture :: String -> IO GLuint
loadTexture fname = do
    dynImage <- getDynImage fname
    let ipixelrgb8 = convertRGB8 dynImage
        iWidth = fromIntegral $ imageWidth ipixelrgb8
        iHeight = fromIntegral $ imageHeight ipixelrgb8
        iData = imageData ipixelrgb8

    VS.unsafeWith iData (makeOpenGLTexture iWidth iHeight)

makeOpenGLTexture :: GLsizei -> GLsizei -> Ptr a -> IO GLuint
makeOpenGLTexture  w h ptr = do
    textureP <- malloc
    glGenTextures 1 textureP
    texture <- peek textureP

    glBindTexture GL_TEXTURE_2D texture
    glTexImage2D GL_TEXTURE_2D 0 GL_RGB w h 0 GL_RGB GL_UNSIGNED_BYTE (castPtr ptr)

    glGenerateMipmap GL_TEXTURE_2D
    glBindTexture GL_TEXTURE_2D 0

    pure texture
