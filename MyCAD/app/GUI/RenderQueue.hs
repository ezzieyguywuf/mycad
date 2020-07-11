module RenderQueue
(
  RenderQueue
, initRenderQueue
, getObjectQueue
, getCameraQueue
, queueCamera
, queueObject
)
where
-- base
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue, newTQueue, writeTQueue)

-- internal imports
import GraphicData (ObjectData)
import ViewSpace   (CameraData)

-- | This will manage any queue that should trigger a re-render
data RenderQueue =
    RenderQueue
        { getObjectQueue :: TQueue ObjectData
        , getCameraQueue :: TQueue CameraData
        }

initRenderQueue :: CameraData -> IO RenderQueue
initRenderQueue camera = do
    objectQueue <- atomically newTQueue
    cameraQueue <- atomically newTQueue
    atomically $ writeTQueue cameraQueue camera
    pure $ RenderQueue objectQueue cameraQueue

-- | Adds the given "CameraData" to the render queue, to be processed later
queueCamera :: RenderQueue -> CameraData -> IO ()
queueCamera queue cData = atomically $ writeTQueue cQueue cData
    where cQueue = getCameraQueue queue

-- | Adds the given "ObjectData" to the render queue, to be processed later
queueObject :: RenderQueue -> ObjectData -> IO ()
queueObject queue oData = atomically $ writeTQueue oQueue oData
    where oQueue = getObjectQueue queue

