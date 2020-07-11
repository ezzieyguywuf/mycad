module Pictures
(
  loadRGB8
)
where

-- third party
import Codec.Picture ( readImage
                     , generateImage
                     --, convertRGB8
                     , DynamicImage(..)
                     , Image(..)
                     , PixelRGB8(..))

-- TODO: This will need to be fleshed out to be used by _loadTexture in
--       GL_Helpers. It probably makes the most sense to directly store the
--       GL-data types needed, e.g. GLint
--data GLImage = GLImage { getData :: }

-- TODO: this will probably want to be `loadRGB8 :: String -> GLImage`
loadRGB8 = getDynImage

-- | Loads an image from a file - if it can't find it, loads a gradient instead
getDynImage :: String -> IO DynamicImage
getDynImage fname = do
    check <- readImage fname
    case check of
        Left msg -> pure generateGradient
        Right rawData -> pure rawData

-- | Generates a gradient.
generateGradient :: DynamicImage
generateGradient = ImageRGB8 $ generateImage renderer 800 600
    where renderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128

