-- import Codec.Picture
import Codec.Picture.Saving
import Codec.Picture.Jpg( decodeJpeg )

import System.Environment( getArgs )
import Control.Applicative( (<$>) )

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

main :: IO ()
main = do
    [inJPG, outBMP] <- getArgs
    jpgData <- B.readFile inJPG

    let (Right bmpData) = imageToBitmap <$> decodeJpeg jpgData
    L.writeFile outBMP bmpData
