{-# LANGUAGE PatternSynonyms #-}
module GL_Renderer
(
  Renderer
, initRenderer
, renderIfNecessary
)where
-- base
import Data.Bits ((.|.))
import Data.Foldable (for_)
import Control.Monad (when, join, foldM)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TQueue (writeTQueue, flushTQueue)
import System.FilePath ((</>))
import Foreign (nullPtr)

-- Third-party
import Graphics.GL.Core33 ( pattern GL_TRIANGLES, pattern GL_UNSIGNED_INT
                          , pattern GL_COLOR_BUFFER_BIT, pattern GL_DEPTH_BUFFER_BIT
                          , pattern GL_DEPTH_TEST
                          , glEnable, glUseProgram, glBindVertexArray, glDrawElements
                          , glClearColor, glClear)
import Graphics.GL.Types (GLuint)

-- Internal
import GL_Helpers (Shader(..), makeShader, putGraphicData, putUniform, makeUniform)
import GraphicData (ObjectData(..), getElementIndices)
import ViewSpace (CameraData, putProjectionUniform)
import RenderQueue (RenderQueue(..))

-- | A renderer contains all of the data needed to render something
data Renderer =
    Renderer
        { _shader  :: Shader
        , _targets :: [RenderTarget]
        , _queue   :: RenderQueue
        }

-- | This contains the actual Vertex data to render. This is not exported
data RenderTarget =
    RenderTarget
        { _getVAO        :: GLuint
        , _getObjectData :: ObjectData
        }

lvshaderFPATH = "MyCAD" </> "GUI" </> "LineVShader.glsl"
fshaderFPATH  = "MyCAD" </> "GUI" </> "FragmentShader.glsl"

-- | This will initialize a Renderer, which can later be used to draw things
initRenderer :: RenderQueue -> Float -> Float -> IO Renderer
initRenderer queue aspectRatio lineThickness = do
    -- Compile our shader
    shader <- makeShader lvshaderFPATH fshaderFPATH

    -- Set our static Uniform variables
    putUniform shader (makeUniform "aspect" aspectRatio)
    putUniform shader (makeUniform "thickness" lineThickness)
    putProjectionUniform aspectRatio shader

    let renderer    = Renderer shader [] queue

    -- enable depth testing
    glEnable GL_DEPTH_TEST

    pure renderer

-- | Will determine if it is necessary to Render, and then do it as needed. The
--   "Renderer" returned may be different than the one passed in, i.e. if an
--   "ObjectData" was queued to be rendered
renderIfNecessary :: Renderer -> IO Renderer
renderIfNecessary renderer = join $ atomically (checkQueues renderer)

-- | Determines the correct "IO" action to take given the state of our Queues
checkQueues :: Renderer -> STM (IO Renderer)
checkQueues renderer = do
    let objectQueue = getObjectQueue (_queue renderer)
        cameraQueue = getCameraQueue (_queue renderer)

    objects <- flushTQueue objectQueue
    cameras <- flushTQueue cameraQueue

    pure $ do -- this is IO
        renderer'  <- foldM addObject renderer objects
        for_ cameras (updateView renderer')

        when (not (null objects) || not (null cameras)) (render renderer')

        pure renderer'

-- | Adds an "ObjectData" to our renderer
addObject :: Renderer -> ObjectData -> IO Renderer
addObject (Renderer shader targets queue) oData = do
    vao <- putGraphicData oData
    let target = RenderTarget vao oData
    pure $ Renderer shader (target : targets) queue

-- | Updates the view matrix using the provided "CameraData"
updateView :: Renderer -> CameraData -> IO ()
updateView renderer cData =
    atomically $ writeTQueue (getCameraQueue (_queue renderer)) cData


-- | This will render every "ObjectData" that has been added to the "Renderer"
render :: Renderer -> IO ()
render (Renderer shader targets _) = do
    -- First, clear what was there
    glClearColor 0.2 0.3 0.3 1.0
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    -- Next, we have to make sure our "Shader" is active
    glUseProgram (_shaderID shader)

    -- Render each target
    mapM_ (renderTarget shader) targets

    -- swap the buffers
    --swapBuffers window

renderTarget :: Shader -> RenderTarget -> IO ()
renderTarget shader rtarget = do
    let vao    = _getVAO rtarget
        oData  = _getObjectData rtarget
        eData  = getElementData oData
        pDatas = getPlacementDatas oData
        len    = fromIntegral $ length (getElementIndices eData)
    -- bind the Vertex Attribute Object, which (among other things)
    -- contains the memory location on the GPU where the Vertex data is
    -- stored
    glBindVertexArray vao
    -- render each "Placement"
    sequence_ $ flip fmap pDatas (\placement -> do
        -- A single Placement specifies the \"model\" transformation matrix
        -- for the item in question
        putUniform shader (makeUniform "model" placement)
        -- Finally, draw the actual triangles
        -- TODO: Update this function to draw things other than just
        --       GL_TRIANGLES
        glDrawElements GL_TRIANGLES len GL_UNSIGNED_INT nullPtr
        )
    glBindVertexArray 0
