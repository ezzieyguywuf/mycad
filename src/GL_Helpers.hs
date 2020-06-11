module GL_Helpers
( makeVertices 
, putGraphicData
, loadShader
, linkShadersToProgram
, loadTexture
, mapTextureUnit
, putMatrix
)where

-- base
import Control.Monad (when)

-- gl, all types and funcs here will already start with "gl"
import Graphics.GL.Core33
import Graphics.GL.Types

-- For loading images
import Codec.Picture ( readImage
                     , generateImage
                     , convertRGB8
                     , DynamicImage(..)
                     , Image(..)
                     , PixelRGB8(..))

-- For maths
import Linear.Matrix

-- This is for the Foreign Function Interface, ffi. This calls C-code
import Foreign

-- Converts Haskell strings to C-strings
import Foreign.C.String (withCAStringLen, newCString)

-- For doing silly things with vector pointers
import qualified Data.Vector.Storable as VS

import GraphicData

-- This sequence is performed often enough it's worth wrapping. The argument it
-- takes it a partially applied glGenSomething function, where we'll provide
-- the pointer and return the address
getNewBufferID :: (Ptr GLuint -> IO ()) -> IO (GLuint)
getNewBufferID f = do
    -- Haskell will use type inference to figure out what kind of pointer
    pointer <- malloc
    -- the openGL function will fill in our pointer for us
    f pointer
    -- return back the dereferenced pointer, with the UID that we can use in our program
    peek pointer

putGraphicData :: GraphicData -> [Int] -> IO GLuint
putGraphicData gdata indices = do
    -- First, make a Vertex Buffer Object. This is a place in openGL's memory
    -- where we can put all of our vertex data
    vbo <- getNewBufferID $ glGenBuffers 1

    -- Next, we're going to create a Vertex Array Object, or VAO, which allows
    -- to reuse the data in our VBO over and over (or something like that)
    vao <- getNewBufferID $ glGenVertexArrays 1


    -- OpenGL needs to know the size of the data we're going to give it
    let dataSize = getDataSize gdata
        flatData = flattenData gdata

    -- This makes a pointer to our data
    dataPointer <- newArray flatData

    -- In openGL, you must "bind" a buffer before you are able to do things to
    -- it. In the case of a VAO, it must be bound before the VBO that is going
    -- to store the actual data
    glBindVertexArray vao
    glBindBuffer GL_ARRAY_BUFFER vbo

    -- Finally write the data. This applies to the currently "bound"
    -- GL_ARRAY_BUFFER, in our case, vbo
    glBufferData GL_ARRAY_BUFFER dataSize (castPtr dataPointer) GL_STATIC_DRAW

    -- Since the Shaders, which use our vertex information, are flexible, we
    -- need to specify to OpenGL how our data is laid out. Did you notice how
    -- we flattened the data earlier? That's how OpenGL expects to recieve it.
    -- But, afterwards, it needs to know how to unflatten it.
    let row = head gdata
        rowSize = getRowSize row
    sequence $ fmap (registerVertexAttribute rowSize) row

    pure vao

registerVertexAttribute :: GLsizei -> VertexAttribute -> IO ()
registerVertexAttribute stride attrib = do
    let i = getIndex attrib
        len = fromIntegral $ length (squashAttribute attrib) :: GLint
    -- First, specify the attribute:
    -- i = index
    -- len = length
    -- GL_FLOAT = type
    -- GL_FALSE = don't normalize
    -- stride = distance between subsequent attributes of the same kind
    -- nullPtr = not sure
    glVertexAttribPointer i len GL_FLOAT GL_FALSE stride nullPtr

    -- Now we have to enable this attribute, per its index
    glEnableVertexAttribArray i

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
        nData     = 5

    -- position attribute
    glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (nData * floatSize) nullPtr
    glEnableVertexAttribArray 0

    -- color attribute
    {-let offset = castPtr $ plusPtr nullPtr (fromIntegral $ 3*floatSize)-}
    {-glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE (nData * floatSize) offset-}
    {-glEnableVertexAttribArray 1-}

    -- texture attribute
    let sixFloatOffset = castPtr $ plusPtr nullPtr (fromIntegral $ 3*floatSize)
    glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE (nData * floatSize) sixFloatOffset
    glEnableVertexAttribArray 1


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

loadTexture :: String -> IO GLuint
loadTexture fname = do
    dynImage <- getDynImage fname
    let ipixelrgb8 = convertRGB8 dynImage
        iWidth = fromIntegral $ imageWidth ipixelrgb8
        iHeight = fromIntegral $ imageHeight ipixelrgb8
        iData = imageData ipixelrgb8

    VS.unsafeWith iData (makeOpenGLTexture iWidth iHeight)

mapTextureUnit :: GLuint -> Int32 -> String -> IO ()
mapTextureUnit shaderProgram target name= do
    -- Find out where in the program the uniform is located
    cname <- newCString name
    loc <- glGetUniformLocation shaderProgram cname

    -- We must call UseProgram before glUniformX, otherwise it won't
    -- know which program to put it in!
    glUseProgram shaderProgram

    -- Bind the texture location to the target index
    glUniform1i loc target

putMatrix :: GLuint -> M44 Float -> String -> IO ()
putMatrix shaderProgram transMatrix name = do
    transP <- malloc
    poke transP transMatrix

    name <- newCString name
    loc <- glGetUniformLocation shaderProgram name

    glUniformMatrix4fv loc 1 GL_FALSE (castPtr transP)

------------------------------------------------------------------
--          Private Free Functions
------------------------------------------------------------------


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

getDynImage :: String -> IO DynamicImage
getDynImage fname = do
    check <- readImage fname
    case check of
        Left msg -> pure generateGradient
        Right rawData -> pure rawData

generateGradient :: DynamicImage
generateGradient = ImageRGB8 $ generateImage renderer 800 600
    where renderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128
