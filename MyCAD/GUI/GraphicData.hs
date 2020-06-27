module GraphicData
(
  VertexAttribute(..)
, AttributeData(..)
, DataRow
, GraphicData(..)
, ElementData(..)
, ObjectData (..)
, PlacementData (..)
, getDataSize
, flattenData
, makeGraphicData
, makeElementData
, placeElement
, addInstance
, getRowData
, rotateElement
)where

import Graphics.GL.Types
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Quaternion
import Foreign
import Data.Foldable
import Data.List

-- | A VertexAttribute describes a single piece of data that OpenGL will read
data VertexAttribute =
     Position (V3 Float)
   | Direction (V3 Float)
   | Color (V4 Float)
   | Texture (V2 Float)
   | Up Float
    deriving (Show)

type DataRow = [VertexAttribute]

data GraphicData = GraphicData {
                                   getDataAttributes :: [AttributeData]
                                 , getData :: [DataRow]
                                 } deriving (Show)

data ElementData  = ElementData {
                                  getGraphicData :: GraphicData
                                , getElementIndices :: [GLuint]
                                }
                                deriving (Show)

data PlacementData = PlacementData { placementRotation    :: Quaternion Float
                                   , placementTranslation :: V3 Float}
                                   deriving (Show)

data ObjectData = ObjectData ElementData [PlacementData] deriving (Show)

data AttributeData = AttributeData
    {
      getIndex :: GLuint
    , getAttribSize :: GLint
    , getStride ::  GLsizei
    , getOffset :: Ptr ()
    }
    deriving (Show)

makeGraphicData :: [DataRow] -> GraphicData
makeGraphicData rows = GraphicData attribData rows
    where attribData = getRowData $ head rows

makeElementData :: [DataRow] -> [GLuint] -> ElementData
makeElementData rows indices = ElementData (makeGraphicData rows) indices

placeElement :: ElementData -> PlacementData -> ObjectData
placeElement edata placement = ObjectData edata [placement]

addInstance :: ObjectData -> PlacementData -> ObjectData
addInstance (ObjectData edata p0) p = ObjectData edata (p : p0)

rotateElement :: ObjectData -> Quaternion Float -> ObjectData
rotateElement (ObjectData edata pdatas) rot = ObjectData edata' pdatas
    where (ElementData gdata indices) = edata
          edata' = ElementData gdata' indices
          (GraphicData adata rows) = gdata
          gdata' = GraphicData adata rows'
          rows' = map (\x -> map (_rotateAttribute rot) x) rows

_rotateAttribute :: Quaternion Float -> VertexAttribute -> VertexAttribute
_rotateAttribute rot (Position v) = Position $ Linear.Quaternion.rotate rot v
_rotateAttribute _ x = x


-- | Note: If you add more VertexAttributes, you must also specify and index
--         for them. This is the index that your Shader will use 
--
--         Also note that even if you have two VertexAttributes that are the
--         same underyling data type, say Position V3 Int and Color V3 Int, you
--         would still need two distinct VertexAttributes values.
getAttributeIndex :: VertexAttribute -> GLuint
getAttributeIndex (Position _) = 0
getAttributeIndex (Direction _) = 1
getAttributeIndex (Color _)    = 2
getAttributeIndex (Up _)       = 3
getAttributeIndex (Texture _)  = 4

-- | This will return a list of data suitable for making calls to glVertexAttribPointer
getRowData :: DataRow -> [AttributeData]
getRowData row = snd $ mapAccumL (makeData $ stride) 0 row
    where stride = fromIntegral $ getRowSize row :: GLsizei

-- | Returns the size of the entire GraphicData in a manner suitable to use in glBufferData
getDataSize :: ElementData -> GLsizeiptr
getDataSize eData = fromIntegral $ nRows * rowSize
    where rows = getData $ getGraphicData eData
          nRows = length rows
          rowSize = fromIntegral $ getRowSize $ head rows

-- | Flattens a GraphicData into a form that OpenGl can (almost) understand
flattenData :: ElementData -> [GLfloat]
flattenData eData = concat (fmap squashRow rows)
    where rows = getData $ getGraphicData eData

----------------------------------------------------------------------------
--                  Private Free Functions
----------------------------------------------------------------------------
-- | This will construct a single AttributeData
--   
--   We can't know the @stride@ ahead of time, since it depends on how many
--   'VertexAttribute' are used in each 'DataRow'
--
--  This function is meant to be used with mapAccumL
makeData :: GLsizei                 -- ^ The stride
            -> Int                  -- ^ The cumulative offset
            -> VertexAttribute      -- ^ The VertexAttribute for which to build the AttributeData
            -> (Int, AttributeData)
makeData stride cumOffset attrib = (cumOffset + len, attribData)
    where  index = getAttributeIndex attrib
           len   = length $ squashAttribute attrib
           attribSize = fromIntegral len :: GLint
           offset = makeOffset cumOffset
           attribData = AttributeData index attribSize stride offset

-- | Flattens out a VertexAttribute in a manner suitable for OpenGL to read
squashAttribute :: VertexAttribute -> [GLfloat]
squashAttribute (Position v) = toList v
squashAttribute (Direction v) = toList v
squashAttribute (Color v) = toList v
squashAttribute (Texture v) = toList v
squashAttribute (Up v) = [v]

-- | A helper, flattens out an entire 'DataRow' using 'squashAttribute'
squashRow :: DataRow -> [GLfloat]
squashRow row = concat $ fmap squashAttribute row

-- | Returns the size of the row in a manner suitable to use in glVertexAttribPointer
getRowSize :: DataRow -> GLsizei
getRowSize gdata = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length $ squashRow gdata)

-- | Generates an offset pointer suitable for use in glVertexAttribPointer
makeOffset :: Int -> Ptr ()
makeOffset n = castPtr $ plusPtr nullPtr (fromIntegral $ n*floatSize)
    where floatSize = sizeOf (0.0::GLfloat)
