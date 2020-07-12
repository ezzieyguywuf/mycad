{-# LANGUAGE PatternSynonyms #-}
module GUI.GL.RenderData
(
  RenderData(..)
, RenderTarget(..)
, initRenderData
)where
-- base
import System.FilePath ((</>))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar)

-- Third-party
import Graphics.GL.Core33 ( pattern GL_DEPTH_TEST, glEnable)
import Graphics.GL.Types (GLuint)

-- Internal
import GUI.GL.Helpers (Shader(..), makeShader, putUniform, makeUniform)
import GUI.GraphicData (ObjectData(..))
import GUI.ViewSpace (putProjectionUniform)
import GUI.RenderQueue (RenderQueue)

-- | A RenderData contains all of the data needed to render something
data RenderData =
    RenderData
        { _shader  :: Shader
        , _targets :: TVar [RenderTarget]
        , _queue   :: RenderQueue
        }

-- | This contains the actual Vertex data to render. This is not exported
data RenderTarget =
    RenderTarget
        { _getVAO        :: GLuint
        , _getObjectData :: ObjectData
        }

lvshaderFPATH = "MyCAD" </> "app" </> "GUI" </> "LineVShader.glsl"
fshaderFPATH  = "MyCAD" </> "app" </> "GUI" </> "FragmentShader.glsl"

-- | This will initialize a RenderDat, which can later be used to Render things
initRenderData :: RenderQueue -> Float -> Float -> IO RenderData
initRenderData queue aspectRatio lineThickness = do
    -- Compile our shader
    shader <- makeShader lvshaderFPATH fshaderFPATH

    -- Set our static Uniform variables
    putUniform shader (makeUniform "aspect" aspectRatio)
    putUniform shader (makeUniform "thickness" lineThickness)
    putProjectionUniform aspectRatio shader

    targets <- atomically $ newTVar []
    let rData = RenderData shader targets queue

    -- enable depth testing
    glEnable GL_DEPTH_TEST

    pure rData

