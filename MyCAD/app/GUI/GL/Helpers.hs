{-# LANGUAGE FlexibleInstances #-}
module GUI.GL.Helpers
(
  Shader(..)
, makeUniform
, makeShader
, putUniform
, putGraphicData
)where

-- base
import Control.Monad (when)
import Foreign ( Ptr, castPtr, sizeOf
               , malloc, alloca, allocaBytes
               , poke, peek
               , newArray, peekArray, mallocArray, withArray
               )
import Foreign.Storable (Storable)
import Foreign.C.String (withCAStringLen, newCString)

-- Third party
import Graphics.GL.Core33
import Graphics.GL.Types (GLuint, GLint, GLsizei, GLenum)
import Linear.Matrix (M44, mkTransformation)

-- internal
import GUI.GraphicData ( ObjectData(..)
                       , ElementData(..)
                       , PlacementData(..)
                       , AttributeData(..)
                       , getDataSize, flattenData, getDataAttributes)

-- | This will store the data necessary to execute a shader and draw something
newtype Shader = Shader { _shaderID       :: GLuint
                        }
-- | This data type encapsulates a glUniform name, and some data to go along with it
--
--   Note that in order for this to be useful with, say, "putUniform", @a@ must
--   be an instance of  "GLUiniform".
data Uniform a = Uniform { _uniformName :: String
                         , _uniformData :: a
                         }

-- | A typeclass that describes how to take an arbitrary piece of data @a@ and
--   load it into an openGL context as a \"Uniform\"
class GLUniform a where
    putData :: GLint -> a -> IO ()

-- | Creates a Shader that can be used to draw things
makeShader :: String        -- ^ Vertex Shader, path to a file
              -> String     -- ^ Fragment Shader, path to a file
              -> IO Shader  -- ^ The Shader can be used to draw things
makeShader vpath fpath = do
    vshader <- readFile vpath >>= loadShader GL_VERTEX_SHADER
    fshader <- readFile fpath >>= loadShader GL_FRAGMENT_SHADER
    uid     <- linkShadersToProgram vshader fshader

    -- I guess these aren't needed any more?
    glDeleteShader vshader
    glDeleteShader fshader

    pure $ Shader uid

-- | Create a uniform with the given name and data
makeUniform :: GLUniform a
               => String     -- ^ The name of the uniform
               -> a          -- ^ The data
               -> Uniform a
makeUniform = Uniform

-- | Tries to load the uniform data with the given name.
--
--   Since the openGL program will still compile and load, even with a missing
--   uniform, we don't return a "Maybe" here. Rather, we print a useful mesage
--   to stdout and let someone else worry about it.
putUniform :: GLUniform a => Shader -> Uniform a -> IO ()
putUniform shader (Uniform name uniformData) = do
    let sid = _shaderID shader

    glUseProgram sid

    cName <- newCString name
    loc <- glGetUniformLocation sid cName

    case compare loc 0 of
        LT -> putStrLn $ "Uniform with name '" <> name <> "' was not found"
        _  -> putData loc uniformData

------------------------------------------------------------------
--          Private Free Functions
------------------------------------------------------------------
instance GLUniform Float where
    putData = glUniform1f

instance GLUniform (M44 Float) where
    putData uid transformationMatrix = do
        transP <- malloc
        poke transP transformationMatrix

        glUniformMatrix4fv uid 1 GL_FALSE (castPtr transP)

instance GLUniform PlacementData where
    putData uid (PlacementData rot trans) = do
        let mat = mkTransformation rot trans
        putData uid mat

-- TODO: If you want to use this, you'll need to finish fleshing out the
--       Pictures module.
--import qualified Data.Vector.Storable as VS
--_loadTexture :: String -> IO GLuint
--_loadTexture fname = do
    --dynImage <- getDynImage fname
    --let ipixelrgb8 = convertRGB8 dynImage
        --iWidth = fromIntegral $ imageWidth ipixelrgb8
        --iHeight = fromIntegral $ imageHeight ipixelrgb8
        --iData = imageData ipixelrgb8

    --VS.unsafeWith iData (makeOpenGLTexture iWidth iHeight)

_makeOpenGLTexture :: GLsizei -> GLsizei -> Ptr a -> IO GLuint
_makeOpenGLTexture  w h ptr = do
    texture <- getPointerVal (glGenTextures 1)

    glBindTexture GL_TEXTURE_2D texture
    glTexImage2D GL_TEXTURE_2D 0 GL_RGB w h 0 GL_RGB GL_UNSIGNED_BYTE (castPtr ptr)

    glGenerateMipmap GL_TEXTURE_2D
    glBindTexture GL_TEXTURE_2D 0

    pure texture

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
            logBytes <- allocaBytes (fromIntegral logLen) $ \logP ->
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
            putStrLn $ prefix ++ " Shader Error:" ++ map (toEnum.fromEnum) logBytes
            pure 0

linkShadersToProgram :: GLuint -> GLuint -> IO GLuint
linkShadersToProgram shader1 shader2 = do
    shaderProgram <- glCreateProgram

    glAttachShader shaderProgram shader1
    glAttachShader shaderProgram shader2
    glLinkProgram shaderProgram

    linkingSuccess <- getPointerVal (glGetProgramiv shaderProgram GL_LINK_STATUS)
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

-- This sequence is performed often enough it's worth wrapping. The argument it
-- takes it a partially applied glGenSomething function, where we'll provide
-- the pointer and return the address
getPointerVal :: Storable a => (Ptr a -> IO ()) -> IO a
getPointerVal f = do
    -- Haskell will use type inference to figure out what kind of pointer
    pointer <- malloc
    -- the openGL function will fill in our pointer for us
    f pointer
    -- return back the dereferenced pointer, with the UID that we can use in our program
    peek pointer

registerVertexAttribute :: AttributeData -> IO ()
registerVertexAttribute d = do
    let i    = getIndex d
        size = getAttribSize d
        stride = getStride d
        offset = getOffset d
    -- Parameters are:
    -- i = index
    -- len = length
    -- GL_FLOAT = type
    -- GL_FALSE = don't normalize
    -- stride = distance between subsequent attributes of the same kind
    -- nullPtr = not sure
    glVertexAttribPointer i size GL_FLOAT GL_FALSE stride offset

    -- Now we have to enable this attribute, per its index
    glEnableVertexAttribArray i

registerElementBufferObject :: GLuint -> [GLuint] -> IO ()
registerElementBufferObject vao indices = do
    -- Prep the indices for use in the EBO
    let indicesSize = fromIntegral $ sizeOf (0 :: GLuint) * length indices
    indicesP <- newArray indices

    -- The Element Buffer Object, or EBO, allows us to re-use vertices in the Buffer. this
    -- let's us save space on the graphics memory.
    -- We sould do this after the VAO has been bound, because then ith VAO will
    -- automatically store a reference to this EBO
    ebo <- getPointerVal $ glGenBuffers 1
    glBindVertexArray vao
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
    glBufferData GL_ELEMENT_ARRAY_BUFFER indicesSize (castPtr indicesP) GL_STATIC_DRAW

putGraphicData :: ObjectData -> IO GLuint
putGraphicData (ObjectData eData _) = do
    -- First, make a Vertex Buffer Object. This is a place in openGL's memory
    -- where we can put all of our vertex data
    vbo <- getPointerVal $ glGenBuffers 1

    -- Next, we're going to create a Vertex Array Object, or VAO, which allows
    -- to reuse the data in our VBO over and over (or something like that)
    vao <- getPointerVal $ glGenVertexArrays 1

    -- OpenGL needs to know the size of the data we're going to give it
    let dataSize = getDataSize eData
        flatData = flattenData eData

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

    -- Register the Vertex Attribute with OpenGL. Since the Shaders, which use
    -- our vertex information, are flexible, we need to specify to OpenGL how
    -- our data is laid out. Did you notice how we flattened the data earlier?
    -- That's how OpenGL expects to recieve it.  But, afterwards, it needs to
    -- know how to unflatten it.
    mapM_ registerVertexAttribute (getDataAttributes . getGraphicData $ eData)
    -- Finally, register the indices in the Element Buffer Object
    registerElementBufferObject vao (getElementIndices eData)

    pure vao
