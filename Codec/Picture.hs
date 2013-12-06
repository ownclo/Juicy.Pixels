{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
-- | Main module for image import/export into various image formats.
--
-- To use the library without thinking about it, look after 'decodeImage' and
-- 'readImage'.
--
-- Generally, the read* functions read the images from a file and try to decode
-- it, and the decode* functions try to decode a bytestring.
--
-- For an easy image writing use the 'saveBmpImage', 'saveJpgImage' & 'savePngImage'
-- functions
module Codec.Picture ( 
                     -- * Generic functions
                       readImage
                     , decodeImage
                     , pixelMap
                     , generateImage
                     , generateFoldImage
                     , withImage

                     -- * Generic image writing
                     , saveBmpImage
                     , saveJpgImage

                     -- * Specific image format functions
                     -- ** Bitmap handling 
                     , BmpEncodable
                     , writeBitmap
                     , encodeBitmap
                     , readBitmap
                     , decodeBitmap
                     , encodeDynamicBitmap 
                     , writeDynamicBitmap 

                     -- ** Jpeg handling
                     , readJpeg
                     , decodeJpeg 
                     , encodeJpeg
                     , encodeJpegAtQuality

                     -- * Image types and pixel types
                     -- ** Image
                     , Image( .. )
                     , DynamicImage( .. )
                     -- ** Pixels
                     , Pixel( .. )
                     -- $graph
                     , Pixel8

                     , PixelRGB8( .. )
                     , PixelYCbCr8( .. )
                     ) where

import Control.Applicative( (<$>) )
import Control.DeepSeq( NFData, deepseq )
import qualified Control.Exception as Exc ( catch, IOException )
import Codec.Picture.Bitmap( BmpEncodable, decodeBitmap
                           , writeBitmap, encodeBitmap
                           , encodeDynamicBitmap, writeDynamicBitmap )
import Codec.Picture.Jpg( decodeJpeg, encodeJpeg, encodeJpegAtQuality )
import Codec.Picture.Saving
import Codec.Picture.Types
#ifdef WITH_MMAP_BYTESTRING
import System.IO.MMap ( mmapFileByteString )
#endif

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

-- | Return the first Right thing, accumulating error
eitherLoad :: c -> [(String, c -> Either String b)] -> Either String b
eitherLoad v = inner ""
    where inner errAcc [] = Left $ "Cannot load file\n" ++ errAcc
          inner errAcc ((hdr, f) : rest) = case f v of
                Left  err  -> inner (errAcc ++ hdr ++ " " ++ err ++ "\n") rest
                Right rez  -> Right rez

withImageDecoder :: (NFData a)
                 => (B.ByteString -> Either String a) -> FilePath
                 -> IO (Either String a)
withImageDecoder decoder path = Exc.catch doit
                    (\e -> return . Left $ show (e :: Exc.IOException))
    where doit = force . decoder <$> get
#ifdef WITH_MMAP_BYTESTRING
          get = mmapFileByteString path Nothing
#else
          get = B.readFile path
#endif
          -- force appeared in deepseq 1.3, Haskell Platform
          -- provide 1.1
          force x = x `deepseq` x

-- | Load an image file without even thinking about it, it does everything
-- as 'decodeImage'
readImage :: FilePath -> IO (Either String DynamicImage)
readImage = withImageDecoder decodeImage

-- | If you want to decode an image in a bytestring without even thinking
-- in term of format or whatever, this is the function to use. It will try
-- to decode in each known format and if one decoding succeed will return
-- the decoded image in it's own colorspace
decodeImage :: B.ByteString -> Either String DynamicImage
decodeImage str = eitherLoad str [("Jpeg", decodeJpeg)
                                 ,("Bitmap", decodeBitmap)
                                 ]

-- | Try to load a jpeg file and decompress. The colorspace is still
-- YCbCr if you want to perform computation on the luma part. You can
-- convert it to RGB using 'colorSpaceConversion'
readJpeg :: FilePath -> IO (Either String DynamicImage)
readJpeg = withImageDecoder decodeJpeg

-- | Try to load a .bmp file. The colorspace would be RGB or RGBA
readBitmap :: FilePath -> IO (Either String DynamicImage)
readBitmap = withImageDecoder decodeBitmap

-- | Save an image to a '.jpg' file, will do everything it can to save an image.
saveJpgImage :: Int -> FilePath -> DynamicImage -> IO ()
saveJpgImage quality path img = L.writeFile path $ imageToJpg quality img

-- | Save an image to a '.bmp' file, will do everything it can to save an image.
saveBmpImage :: String -> DynamicImage -> IO ()
saveBmpImage path img = L.writeFile path $ imageToBitmap img
