module GraphicData
(
  VertexAttribute(..)
, AttributeData(..)
, DataRow
, GraphicData
, getRowData
, squashAttribute
, getDataSize
, squashRow
, flattenData
)where

import Graphics.GL.Types
import Linear.V2
import Linear.V3
import Foreign
import Data.Foldable
import Data.List

data VertexAttribute =
     Position (V3 Float)
   | Texture (V2 Float)
    deriving (Show)

type DataRow = [VertexAttribute]

type GraphicData = [DataRow]

data AttributeData = AttributeData
    {
      getIndex :: GLuint
    , getAttribSize :: GLint
    , getStride ::  GLsizei
    , getOffset :: Ptr ()
    }
    | NoData

-- | Note: If you add more VertexAttributes, you must also specify and index
--         for them. This is the index that your Shader will use 
--
--         Also note that even if you have two VertexAttributes that are the
--         same underyling data type, say Position V3 Int and Color V3 Int, you
--         would still need two distinct VertexAttributes values.
getAttributeIndex :: VertexAttribute -> GLuint
getAttributeIndex (Position _) = 0
getAttributeIndex (Texture _)  = 1

getRowData :: DataRow -> [AttributeData]
getRowData row = snd $ mapAccumL (makeData $ stride) 0 row
    where stride = fromIntegral $ getRowSizeInt row :: GLsizei

makeData :: GLsizei -> Int -> VertexAttribute -> (Int, AttributeData)
makeData stride cumOffset attrib = (cumOffset + len, attribData)
    where  index = getAttributeIndex attrib
           len   = length $ squashAttribute attrib
           attribSize = fromIntegral len :: GLint
           offset = makeOffset cumOffset
           attribData = AttributeData index attribSize stride offset

makeOffset :: Int -> Ptr ()
makeOffset n = castPtr $ plusPtr nullPtr (fromIntegral $ n*floatSize)
    where floatSize = sizeOf (0.0::GLfloat)

squashAttribute :: VertexAttribute -> [GLfloat]
squashAttribute (Position v) = toList v
squashAttribute (Texture v) = toList v

squashRow :: DataRow -> [GLfloat]
squashRow row = concat $ fmap squashAttribute row

getRowSizeInt :: DataRow -> Int
getRowSizeInt gdata = sizeOf (0.0 :: GLfloat) * (length $ squashRow gdata)

getDataSize :: GraphicData -> GLsizeiptr
getDataSize rows = fromIntegral $ nRows * rowSize
    where nRows = length rows
          rowSize = getRowSizeInt $ head rows

flattenData :: GraphicData -> [GLfloat]
flattenData = concat . (fmap squashRow)
