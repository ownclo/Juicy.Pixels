-- test file, don't care about unused, on the contrary...
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Codec.Picture
import Codec.Picture.Jpg( encodeJpeg )
import System.Environment

import Data.Binary
import Data.Monoid
import Data.Word( Word8 )
import Control.Monad( forM_ )
import System.FilePath
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Codec.Picture.Types
import Codec.Picture.Saving
import qualified Data.Vector.Storable as V

import Control.Applicative( (<$>) )
import qualified Criterion.Config as C
import Criterion.Main
import Control.DeepSeq

greyScaleWitness :: Image Pixel8
greyScaleWitness = img 232 241
    where img w h = Image w h $ V.fromListN (w * h) $ pixels w h
          pixels w h = [pixel x y | y <- [0 .. h-1], x <- [0 .. w-1] ]
          pixel x y = truncate $ sqrt dist
                where xf = fromIntegral $ x - 100 :: Int
                      yf = fromIntegral $ y - 100
                      dist = (fromIntegral $ xf * xf + yf * yf) :: Double

jpegValidTests :: [FilePath]
jpegValidTests = [ "explore_jpeg.jpg"
                 , "16x16jpeg.jpg", "8x8jpeg.jpg", "avatar.jpg"
                 , "fenek.jpg", "JPEG_example_JPG_RIP_001.jpg"
                 , "JPEG_example_JPG_RIP_010.jpg", "JPEG_example_JPG_RIP_025.jpg"
                 , "JPEG_example_JPG_RIP_050.jpg", "JPEG_example_JPG_RIP_100.jpg"
                 , "sheep.jpg", "mand_prgrsv.jpg"
                 , "MCU0.jpg", "MCU1.jpg", "MCU5.jpg", "MCU10.jpg", "MCU35.jpg"
                 , "20100713-0107-interleaved2.jpg"
                 ]

bmpValidTests :: [FilePath]
bmpValidTests = ["simple_bitmap_24bits.bmp"]

validationJpegEncode :: Image PixelYCbCr8 -> L.ByteString
validationJpegEncode = encodeJpegAtQuality 100

imgToImg :: FilePath -> IO ()
imgToImg path = do
    rez <- readImage path
    case rez of
        Right (ImageYCbCr8 img) -> do
            let rgb = convertImage img :: Image PixelRGB8
                jpg = validationJpegEncode img
                bmp = encodeBitmap rgb
            putStrLn $ "YCbCr : " ++ path
            putStrLn "-> JPG"
            L.writeFile (path ++ "._fromYCbCr8.jpg") jpg
            putStrLn "-> BMP"
            L.writeFile (path ++ "._fromYCbCr8.bmp") bmp
            putStrLn "-> PNG"

        Right (ImageRGB8 img) -> do
            let jpg = validationJpegEncode (convertImage img)
                bmp = encodeBitmap img
            putStrLn $ "RGB8 : " ++ path
            putStrLn "-> BMP"
            L.writeFile (path ++ "._fromRGB8.bmp") bmp
            putStrLn "-> JPG"
            L.writeFile (path ++ "._fromRGB8.jpg") jpg

        Left err ->
            putStrLn $ "Error loading " ++ path ++ " " ++ err

toJpg :: String -> Image PixelRGB8 -> IO ()
toJpg name img = do
    let jpg = validationJpegEncode (convertImage img)
    putStrLn "-> JPG"
    L.writeFile (name ++ "._fromRGB8.jpg") jpg

radianceTest :: [FilePath]
radianceTest = [ "sunrise.hdr", "free_009.hdr"]

testSuite :: IO ()
testSuite = do
    putStrLn ">>>> Valid instances"
    toJpg "white" $ generateImage (\_ _ -> PixelRGB8 255 255 255) 16 16
    toJpg "black" $ generateImage (\_ _ -> PixelRGB8 0 0 0) 16 16
    toJpg "test" $ generateImage (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) 255)
                                        128 128

    mapM_ (imgToImg . (("tests" </> "bmp") </>)) bmpValidTests
    mapM_ (imgToImg . (("tests" </> "jpeg") </>)) ("huge.jpg" : jpegValidTests)

benchMark :: IO ()
benchMark = do
    putStrLn "Benchmarking"

    hugeJpeg <- B.readFile "tests/jpeg/huge.jpg"

    let myConfig = C.defaultConfig { C.cfgSamples = C.ljust 12 }
    defaultMainWith myConfig (return ()) [
        bgroup "reading"
            [ bench "Huge jpeg" $ nf decodeImage hugeJpeg
            ]
        ]
    putStrLn "END"

-- debug :: IO ()
-- debug = do
--  forM_ ["MCU0.jpg", "MCU1.jpg", "MCU5.jpg", "MCU10.jpg"
--        ,"MCU35.jpg"
--        ,"mand_prgrsv.jpg"
--        ,"sheep.jpg"
--        ,"20100713-0107-interleaved2.jpg"
--        ] $ \file -> do
--     putStrLn "========================================================="
--     putStrLn "========================================================="
--     putStrLn $ "decoding " ++ file
--     img <- readImage $ "tests/jpeg/" ++ file
--     case img of
--         Right i -> savePngImage (file ++ "_debug.png") i
--         Left err -> do
--             putStrLn err
--             {-error "Can't decompress img"-}

myMain :: IO ()
myMain = do
    args <- getArgs
    case args of
        ("test":_) -> testSuite
--         ("debug":_) -> debug
--         ("jpegtopng":_) -> jpegToPng
--         ("pngtojpeg":_) -> pngToJpeg
--         ("pngtobmp":_) -> pngToBmp
        _ -> do
            putStrLn ("Unknown command " ++ show args ++ "Launching benchMark")
            benchMark

main :: IO ()
main = myMain
