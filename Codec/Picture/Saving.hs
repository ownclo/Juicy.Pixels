{-# LANGUAGE TypeFamilies #-}
-- | Helper functions to save dynamic images to other file format
-- with automatic color space/sample format conversion done automatically.
module Codec.Picture.Saving( imageToJpg
                           , imageToBitmap
                           ) where

import qualified Data.ByteString.Lazy as L
import Codec.Picture.Bitmap
import Codec.Picture.Jpg
import Codec.Picture.Types

-- | This function will try to do anything to encode an image
-- as JPEG, make all color conversion and such. Equivalent
-- of 'decodeImage' for jpeg encoding
imageToJpg :: Int -> DynamicImage -> L.ByteString
imageToJpg quality dynImage =
    let encodeAtQuality = encodeJpegAtQuality (fromIntegral quality)
    in case dynImage of
        ImageYCbCr8 img -> encodeAtQuality img
        ImageRGB8   img -> encodeAtQuality (convertImage img)

-- | This function will try to do anything to encode an image
-- as bitmap, make all color conversion and such. Equivalent
-- of 'decodeImage' for Bitmap encoding
imageToBitmap :: DynamicImage -> L.ByteString
imageToBitmap (ImageYCbCr8 img) = encodeBitmap (convertImage img :: Image PixelRGB8)
imageToBitmap (ImageRGB8   img) = encodeBitmap img
