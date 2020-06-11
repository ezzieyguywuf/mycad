module GraphicData
(
  VertexAttribute(..)
, DataRow
, GraphicData
, getIndex
, squashAttribute
, getRowSize
, getDataSize
, squashRow
, flattenData
)where

import Graphics.GL.Types
import Linear.V2
import Linear.V3
import Foreign
import Data.Foldable

data VertexAttribute =
     Position (V3 Float)
   | Texture (V2 Float)
    deriving (Show)

type DataRow = [VertexAttribute]
type GraphicData = [DataRow]

getIndex :: VertexAttribute -> GLuint
getIndex (Position _) = 0
getIndex (Texture _)  = 1

squashAttribute :: VertexAttribute -> [GLfloat]
squashAttribute (Position v) = toList v
squashAttribute (Texture v) = toList v

squashRow :: DataRow -> [GLfloat]
squashRow row = concat $ fmap squashAttribute row

getRowSizeInt :: DataRow -> Int
getRowSizeInt gdata = sizeOf (0.0 :: GLfloat) * (length $ squashRow gdata)

getRowSize :: DataRow -> GLsizei
getRowSize = fromIntegral . getRowSizeInt

getDataSize :: GraphicData -> GLsizeiptr
getDataSize rows = fromIntegral $ nRows * rowSize
    where nRows = length rows
          rowSize = getRowSizeInt $ head rows

flattenData :: GraphicData -> [GLfloat]
flattenData = concat . (fmap squashRow)
