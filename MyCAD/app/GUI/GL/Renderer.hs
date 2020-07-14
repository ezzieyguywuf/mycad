{-# LANGUAGE PatternSynonyms #-}
module GUI.GL.Renderer
(
  renderIfNecessary
)where

-- Base
import Data.Bits ((.|.))
import Data.Foldable (for_)
import Control.Monad (when, join)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TQueue (flushTQueue)
import Control.Concurrent.STM.TVar (readTVar, writeTVar)
import Foreign (nullPtr)

-- Third-party
import Graphics.GL.Core33 ( pattern GL_TRIANGLES, pattern GL_UNSIGNED_INT
                          , pattern GL_COLOR_BUFFER_BIT, pattern GL_DEPTH_BUFFER_BIT
                          , glUseProgram, glBindVertexArray, glDrawElements
                          , glClearColor, glClear)

-- Internal
import GUI.GL.RenderData (RenderData(..), RenderTarget(..))
import GUI.GL.Helpers (Shader(..), putGraphicData, putUniform, makeUniform)
import GUI.RenderQueue (getObjectQueue, getCameraQueue)
import GUI.GraphicData (ObjectData(..), getElementIndices)
import GUI.ViewSpace (CameraData, putViewUniform)
import GUI.GLFW_Helpers (Window, swapBuffers)

-- | Will determine if it is necessary to render, and then do it as needed. The
--   "RenderData" returned may be different than the one passed in, i.e. if an
--   "ObjectData" was queued to be rendered, it is added to the RenderData
renderIfNecessary :: Window -> RenderData -> IO RenderData
renderIfNecessary window renderData = join $ atomically (checkQueues window renderData)

-- | Determines the correct "IO" action to take given the state of our Queues
checkQueues :: Window -> RenderData -> STM (IO RenderData)
checkQueues window renderData = do
    let objectQueue = getObjectQueue (_queue renderData)
        cameraQueue = getCameraQueue (_queue renderData)

    objects <- flushTQueue objectQueue
    cameras <- flushTQueue cameraQueue

    pure $ do -- this is IO
        for_ objects (addObject renderData)
        for_ cameras (updateView renderData)

        when (not (null objects) || not (null cameras)) (render window renderData)

        pure renderData

-- | Adds an "ObjectData" to our "RenderData"
addObject :: RenderData -> ObjectData -> IO ()
addObject rData oData = do
    vao <- putGraphicData oData
    let target = RenderTarget vao oData
    atomically $ do
        let targetsVar = _targets rData
        targets <- readTVar targetsVar
        writeTVar targetsVar (target : targets)

-- | Updates the view matrix using the provided "CameraData"
updateView :: RenderData -> CameraData -> IO ()
updateView renderData cData = putViewUniform cData (_shader renderData)

-- | This will render every "ObjectData" that has been added to the "RenderData"
render :: Window -> RenderData -> IO ()
render window (RenderData shader targetsVar _) = do
    -- First, clear what was there
    glClearColor 0.2 0.3 0.3 1.0
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    -- Next, we have to make sure our "Shader" is active
    glUseProgram (_shaderID shader)

    -- Render each target
    targets <- atomically (readTVar targetsVar)
    mapM_ (renderTarget shader) targets

    -- swap the buffers
    swapBuffers window

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
