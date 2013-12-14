-- import Codec.Picture
import Codec.Picture.Saving
import Codec.Picture.Jpg( decodeJpeg )

import System.Environment( getArgs )
import Control.Applicative( (<$>) )

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

main :: IO ()
main = do
    [inJPG, outBMP] <- getArgs
    jpgData <- B.readFile inJPG

    case imageToBitmap <$> decodeJpeg jpgData of
        Left err -> putStrLn err
        Right bmpData -> L.writeFile outBMP bmpData
