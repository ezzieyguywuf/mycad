module GraphicData
(
-- * Exported Data types
  VertexAttribute(..)
, AttributeData(..)
, GraphicData(..)
, ElementData(..)
, ObjectData (..)
, PlacementData (..)
-- * Exported functions
, flattenData
, makePlacement
, placeElement
, makeElementData
, getDataSize
)where

-- Base
import Foreign (Ptr, sizeOf, castPtr, plusPtr, nullPtr)
import Data.Foldable (toList)
import Data.List (mapAccumL)

-- External
import Graphics.GL.Types (GLuint, GLint, GLsizei, GLfloat, GLsizeiptr)
import Linear.V1 (V1)
import Linear.V2 (V2)
import Linear.V3 (V3)
import Linear.V4 (V4)
import Linear.Quaternion (Quaternion)

-- | A VertexAttribute describes a single piece of data that OpenGL will read
data VertexAttribute =
     Position  (V3 Float)
   | Direction (V3 Float)
   | Color     (V4 Float)
   | Texture   (V2 Float)
   | Up        (V1 Float)
    deriving (Show)

-- | Data is sent to openGL one row at a time
type DataRow = [VertexAttribute]

-- | This identifies all the Vertices that we want to send to OpenGL, as well
--   as the information that OpenGL needs to understand each Vertex
data GraphicData =
    GraphicData
        { getDataAttributes :: [AttributeData]
        , getData :: [DataRow]
        } deriving (Show)

-- | This describes which sequence to read Vertices in
--
--   This is done in order to eliminate duplication - if we have two trangles
--   to draw a rectangle, we can fully describe the rectangle using four
--   vertices. If we didn't use Elements, we'd need to use six vertices
--   instead.
data ElementData =
    ElementData
        { getGraphicData    :: GraphicData
        , getElementIndices :: [GLuint]
        } deriving (Show)

-- | An \"Element\" can be placed in the World zero or more times. This
--   describes a placement
data PlacementData =
    PlacementData
        { placementRotation    :: Quaternion Float
        , placementTranslation :: V3 Float
        } deriving (Show)

-- | Finally, any given \"Object\" that we are going to draw needs to include
--   the data describing the \"Element\" along with its \"Placements\"
data ObjectData =
    ObjectData
        { getElementData    :: ElementData
        , getPlacementDatas :: [PlacementData]
        }deriving (Show)

-- | This describes the information needed to inform OpenGL how to read a
--   single Vertex Attribute
--
--   Note that a \"row\" of data can include one or more Attributes
data AttributeData = AttributeData
    { getIndex :: GLuint
    , getAttribSize :: GLint
    , getStride ::  GLsizei
    , getOffset :: Ptr ()
    }
    deriving (Show)

-- | By placing the "ElementData" in the \"world\", you create an "ObjectData"
--   that can be rendered
placeElement :: ElementData -> PlacementData -> ObjectData
placeElement eData pData = ObjectData eData [pData]

-- | A "Placement" can be described by a translation and a rotation
makePlacement :: Quaternion Float -> V3 Float -> PlacementData
makePlacement rot trans = PlacementData rot trans

-- | The "ElementData" describes a single \"Element\" to be rendered in OpenGL
makeElementData :: [DataRow]       -- ^ The data describing the Element
                   -> [GLuint]     -- ^ The indices to render
                   -> ElementData
makeElementData rows indices = ElementData (makeGraphicData rows) indices

-- | Flattens a GraphicData into a form that OpenGl can (almost) understand
flattenData :: ElementData -> [GLfloat]
flattenData eData = concat (fmap squashRow rows)
    where rows = getData $ getGraphicData eData

-- | Returns the size of the entire GraphicData in a manner suitable to use in glBufferData
getDataSize :: ElementData -> GLsizeiptr
getDataSize eData = fromIntegral $ nRows * rowSize
    where rows = getData $ getGraphicData eData
          nRows = length rows
          rowSize = fromIntegral $ getRowSize $ head rows

----------------------------------------------------------------------------
--                  Private Free Functions
----------------------------------------------------------------------------
makeGraphicData :: [DataRow] -> GraphicData
makeGraphicData rows = GraphicData attribData rows
    where attribData = getRowData $ head rows

-- | This will return a list of data suitable for making calls to glVertexAttribPointer
getRowData :: DataRow -> [AttributeData]
getRowData row = snd $ mapAccumL (makeData $ stride) 0 row'
    where stride = fromIntegral $ getRowSize row :: GLsizei
          row'   = zip [0..] row


-- | This will construct a single AttributeData
--
--   We can't know the @stride@ ahead of time, since it depends on how many
--   'VertexAttribute' are used in each 'DataRow'
--
--  This function is meant to be used with mapAccumL
makeData :: GLsizei                   -- ^ The stride
            -> Int                    -- ^ The cumulative offset
            -- | The VertexAttribute for which to build the AttributeData,
            --   including its index.
            -> (GLuint, VertexAttribute)
            -> (Int, AttributeData)
makeData stride cumOffset (index, attrib) = (cumOffset + len, attribData)
    where  len   = length $ makeList attrib
           attribSize = fromIntegral len :: GLint
           offset = makeOffset cumOffset
           attribData = AttributeData index attribSize stride offset

-- | Or I could make VertexAttribute an instance of Foldable, I guess
makeList :: VertexAttribute -> [Float]
makeList (Position v)  = toList v
makeList (Direction v) = toList v
makeList (Color v)     = toList v
makeList (Texture v)   = toList v
makeList (Up v)        = toList v

-- | A helper, flattens out an entire 'DataRow'
squashRow :: DataRow -> [GLfloat]
squashRow row = concat $ fmap makeList row

-- | Returns the size of the row in a manner suitable to use in glVertexAttribPointer
getRowSize :: DataRow -> GLsizei
getRowSize gdata = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length $ squashRow gdata)

-- | Generates an offset pointer suitable for use in glVertexAttribPointer
makeOffset :: Int -> Ptr ()
makeOffset n = castPtr $ plusPtr nullPtr (fromIntegral $ n*floatSize)
    where floatSize = sizeOf (0.0::GLfloat)

