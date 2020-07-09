{-# LANGUAGE PatternSynonyms #-}
module GL_Renderer
(
  renderIfNecessary
)where

-- Base
import Data.Bits ((.|.))
import Data.Foldable (for_)
import Control.Monad (when, join, foldM)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TQueue (flushTQueue)
import Foreign (nullPtr)

-- Third-party
import Graphics.GL.Core33 ( pattern GL_TRIANGLES, pattern GL_UNSIGNED_INT
                          , pattern GL_COLOR_BUFFER_BIT, pattern GL_DEPTH_BUFFER_BIT
                          , glUseProgram, glBindVertexArray, glDrawElements
                          , glClearColor, glClear)

-- Internal
import GL_RenderData (RenderData(..), RenderTarget(..))
import RenderQueue (getObjectQueue, getCameraQueue)
import GraphicData (ObjectData(..), getElementIndices)
import ViewSpace (CameraData, putViewUniform)
import GL_Helpers (Shader(..), putGraphicData, putUniform, makeUniform)

-- | Will determine if it is necessary to render, and then do it as needed. The
--   "RenderData" returned may be different than the one passed in, i.e. if an
--   "ObjectData" was queued to be rendered, it is added to the RenderData
renderIfNecessary :: RenderData -> IO RenderData
renderIfNecessary renderData = join $ atomically (checkQueues renderData)

-- | Determines the correct "IO" action to take given the state of our Queues
checkQueues :: RenderData -> STM (IO RenderData)
checkQueues renderData = do
    let objectQueue = getObjectQueue (_queue renderData)
        cameraQueue = getCameraQueue (_queue renderData)

    objects <- flushTQueue objectQueue
    cameras <- flushTQueue cameraQueue

    pure $ do -- this is IO
        renderData'  <- foldM addObject renderData objects
        for_ cameras (updateView renderData')

        when (not (null objects) || not (null cameras)) (render renderData')

        pure renderData'

-- | Adds an "ObjectData" to our "RenderData"
addObject :: RenderData -> ObjectData -> IO RenderData
addObject (RenderData shader targets queue) oData = do
    putStrLn "Adding object"
    vao <- putGraphicData oData
    let target = RenderTarget vao oData
    pure $ RenderData shader (target : targets) queue

-- | Updates the view matrix using the provided "CameraData"
updateView :: RenderData -> CameraData -> IO ()
updateView renderData cData = do
    putStrLn "updatingView"
    putViewUniform cData (_shader renderData)

-- | This will render every "ObjectData" that has been added to the "RenderData"
render :: RenderData -> IO ()
render (RenderData shader targets _) = do
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
