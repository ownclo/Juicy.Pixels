import Codec.Picture.Saving
import Codec.Picture.Bitmap( decodeBitmap )

import System.Environment( getArgs )
import Control.Applicative( (<$>) )

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

main :: IO ()
main = do
    [inBmp, outJPG] <- getArgs
    bmpData <- B.readFile inBmp

    let (Right jpgData) = imageToJpg 50 <$> decodeBitmap bmpData
    L.writeFile outJPG jpgData
