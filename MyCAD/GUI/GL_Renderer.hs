{-# LANGUAGE PatternSynonyms #-}
module GL_Renderer
(
  Renderer
, initRenderer
, addObject
, render
, updateView
)where
-- base
import Data.Bits ((.|.))
import System.FilePath ((</>))
import Foreign (nullPtr)

-- Third-party
import Graphics.GL.Core33 ( pattern GL_TRIANGLES, pattern GL_UNSIGNED_INT
                          , pattern GL_COLOR_BUFFER_BIT, pattern GL_DEPTH_BUFFER_BIT
                          , glUseProgram, glBindVertexArray, glDrawElements
                          , glClearColor, glClear)
import Graphics.GL.Types (GLuint)

-- Internal
import GLFW_Helpers (Window, swapBuffers)
import GL_Helpers (Shader(..), makeShader, putGraphicData, putUniform, makeUniform)
import GraphicData (ObjectData(..), getElementIndices)
import ViewSpace (CameraData, putProjectionUniform, putViewUniform)

-- | A renderer contains all of the data needed to render something, except for
--   the actual Vertex data to render
data Renderer =
    Renderer
        { _shader  :: Shader
        , _objects :: [RenderTarget]
        }

-- | This contains the actual Vertex data to render
data RenderTarget =
    RenderTarget
        { _getVAO        :: GLuint
        , _getObjectData :: ObjectData
        }

lvshaderFPATH = "MyCAD" </> "GUI" </> "LineVShader.glsl"
fshaderFPATH  = "MyCAD" </> "GUI" </> "FragmentShader.glsl"

-- | This will initialize a Renderer, which can later be used to draw things
initRenderer :: CameraData -> Float -> Float -> IO Renderer
initRenderer camera aspectRatio lineThickness = do
    shader <- makeShader lvshaderFPATH fshaderFPATH

    putUniform shader (makeUniform "aspect" aspectRatio)
    putUniform shader (makeUniform "thickness" lineThickness)
    putProjectionUniform aspectRatio shader

    let renderer = Renderer shader []
    updateView camera renderer
    pure renderer

    pure $ Renderer shader []

-- | Updates the view matrix using the provided "CameraData"
updateView :: CameraData -> Renderer -> IO ()
updateView camera renderer = do
    putViewUniform camera (_shader renderer)

-- | This adds a renderable "ObjectData" to a renderer. It still does not draw
--   anything
addObject :: Renderer -> ObjectData -> IO Renderer
addObject (Renderer shader targets) oData = do
    vao <- putGraphicData oData
    let target = RenderTarget vao oData
    pure $ Renderer shader (target : targets)

-- | This will render every "ObjectData" that has been adding to the "Renderer"
render :: Window -> Renderer -> IO ()
render window (Renderer shader targets) = do
    -- First, clear what was there
    glClearColor 0.2 0.3 0.3 1.0
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    -- Next, we have to make sure our "Shader" is active
    glUseProgram (_shaderID $ shader)
    sequence_ $ flip fmap targets (\rtarget -> do
        let vao    = _getVAO rtarget
            oData  = _getObjectData rtarget
            eData  = getElementData oData
            pDatas = getPlacementDatas oData
            len    = fromIntegral $ length (getElementIndices eData)
        -- bind the Vertex Attribute Object, which (among other things)
        -- contains the memory location on the GPU where the Vertex data is
        -- stored
        glBindVertexArray vao
        sequence_ $ flip fmap pDatas (\placement -> do
            -- A single Placement specifies the \"model\" transformation matrix
            -- for the item in question
            putUniform shader (makeUniform "model" placement)
            -- Finally, draw the actual triangles
            -- TODO: Update this function to draw things other than just
            --       GL_TRIANGLES
            glDrawElements GL_TRIANGLES len GL_UNSIGNED_INT nullPtr)
        glBindVertexArray 0)

    -- swap the buffers
    swapBuffers window
