{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fspec-constr-count=5 #-}
-- | Module used for JPEG file loading and writing.
module Codec.Picture.Jpg( decodeJpeg
                        , encodeJpegAtQuality
                        , encodeJpeg
                        ) where

import Control.Arrow( (>>>) )
import Control.Applicative( (<$>) )
import Control.Monad( when, forM_, void )
import Control.Monad.ST( ST, runST )
import Control.Monad.Trans( lift )

import Data.Bits( (.|.), unsafeShiftL )
import Data.Int( Int16, Int32 )
import Data.Word(Word8, Word32)
import Data.Binary( encode )

import Data.Vector.Unboxed( (!) )
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Codec.Picture.BitWriter
import Codec.Picture.Types
import Codec.Picture.Jpg.Types
import Codec.Picture.Jpg.Common
import Codec.Picture.Jpg.DefaultTable
import Codec.Picture.Jpg.FastDct
import Codec.Picture.Jpg.Huffman

import Codec.Picture.Jpg.Parser
import Codec.Picture.Jpg.EnvReader

quantize :: MacroBlock Int16 -> MutableMacroBlock s Int32
         -> ST s (MutableMacroBlock s Int32)
quantize table block = update 0
  where update 64 = return block
        update idx = do
            val <- block `M.unsafeRead` idx
            let q = fromIntegral (table `VS.unsafeIndex` idx)
                finalValue = (val + (q `div` 2)) `quot` q -- rounded integer division
            (block `M.unsafeWrite` idx) finalValue
            update $ idx + 1


powerOf :: Int32 -> Word32
powerOf 0 = 0
powerOf n = limit 1 0
    where val = abs n
          limit range i | val < range = i
          limit range i = limit (2 * range) (i + 1)

encodeInt :: BoolWriteStateRef s -> Word32 -> Int32 -> ST s ()
{-# INLINE encodeInt #-}
encodeInt st ssss n | n > 0 = writeBits' st (fromIntegral n) (fromIntegral ssss)
encodeInt st ssss n         = writeBits' st (fromIntegral $ n - 1) (fromIntegral ssss)

-- | Assume the macro block is initialized with zeroes
acCoefficientsDecode :: HuffmanPackedTree -> MutableMacroBlock s Int16
                     -> BoolReader s (MutableMacroBlock s Int16)
acCoefficientsDecode acTree mutableBlock = parseAcCoefficient 1 >> return mutableBlock
  where parseAcCoefficient n | n >= 64 = return ()
                             | otherwise = do
            rrrrssss <- decodeRrrrSsss acTree
            case rrrrssss of
                (  0, 0) -> return ()
                (0xF, 0) -> parseAcCoefficient (n + 16)
                (rrrr, ssss) -> do
                    decoded <- fromIntegral <$> decodeInt ssss
                    lift $ (mutableBlock `M.unsafeWrite` (n + rrrr)) decoded
                    parseAcCoefficient (n + rrrr + 1)

-- | Decompress a macroblock from a bitstream given the current configuration
-- from the frame.
decompressMacroBlock :: HuffmanPackedTree   -- ^ Tree used for DC coefficient
                     -> HuffmanPackedTree   -- ^ Tree used for Ac coefficient
                     -> MacroBlock Int16    -- ^ Current quantization table
                     -> MutableMacroBlock s Int16    -- ^ A zigzag table, to avoid allocation
                     -> DcCoefficient       -- ^ Previous dc value
                     -> BoolReader s (DcCoefficient, MutableMacroBlock s Int16)
decompressMacroBlock dcTree acTree quantizationTable zigzagBlock previousDc = do
    dcDeltaCoefficient <- dcCoefficientDecode dcTree
    block <- lift createEmptyMutableMacroBlock
    let neoDcCoefficient = previousDc + dcDeltaCoefficient
    lift $ (block `M.unsafeWrite` 0) neoDcCoefficient
    fullBlock <- acCoefficientsDecode acTree block
    decodedBlock <- lift $ decodeMacroBlock quantizationTable zigzagBlock fullBlock
    return (neoDcCoefficient, decodedBlock)

decodeJpeg :: B.ByteString -> Either String DynamicImage
decodeJpeg file = do
        (env, rest) <- parseHeader file
        let spec = getImageSpec env
            (iWidth, iHeight) = imgSize_ spec
            compCount = compCount_ spec
            imgSize = iWidth * iHeight * compCount

            pixelData :: VS.Vector Word8
            pixelData = runST $ do
                resultImage <- M.new imgSize
                let wrappedImg = MutableImage iWidth iHeight resultImage
                decodeImage spec rest wrappedImg
                VS.unsafeFreeze resultImage

        return . ImageYCbCr8 $ Image iWidth iHeight pixelData

decodeImage :: ImageSpec
             -> B.ByteString
             -> MutableImage s PixelYCbCr8
             -> ST s ()
-- decodeImage = undefined
decodeImage spec codedData outImage = do
    let (ImageSpec compCount _
                   (iMcuWidth, iMcuHeight)
                   (maxXsf, maxYsf)
                   mcuSpec) = spec

        imgReader = initBoolStateJpg codedData

    (zigZagArray :: MutableMacroBlock s Int16) <- createEmptyMutableMacroBlock
    dcArray <- M.replicate compCount 0 :: ST s (M.STVector s DcCoefficient)

    void $ execBoolReader imgReader $ rasterMap iMcuWidth iMcuHeight $ \x y ->
        forM_ mcuSpec $ \(CompMCUSpec compIdx
                            (duWidth, duHeight)
                            (subX, subY)
                            (DataUnitSpec qTable dcTree acTree)) ->
            rasterMap duWidth duHeight $ \xd yd -> do
                dcprev <- lift $ dcArray `M.unsafeRead` compIdx
                (dcCoeff, block) <- decompressMacroBlock dcTree acTree qTable
                                                         zigZagArray dcprev
                lift $ (dcArray `M.unsafeWrite` compIdx) dcCoeff
                lift $ unpackMacroBlock compCount subX subY compIdx
                                        (x * maxXsf + xd) (y * maxYsf + yd)
                                        outImage block

extractBlock :: Image PixelYCbCr8       -- ^ Source image
             -> MutableMacroBlock s Int16 -- ^ Mutable block where to put extracted block
             -> Int                     -- ^ Plane
             -> Int                     -- ^ X sampling factor
             -> Int                     -- ^ Y sampling factor
             -> Int                     -- ^ Sample per pixel
             -> Int                     -- ^ Block x
             -> Int                     -- ^ Block y
             -> ST s (MutableMacroBlock s Int16)
extractBlock (Image { imageWidth = w, imageHeight = h, imageData = src })
             block 1 1 sampCount plane bx by | (bx * dctBlockSize) + 7 < w && (by * 8) + 7 < h = do
    let baseReadIdx = (by * dctBlockSize * w) + bx * dctBlockSize
    sequence_ [(block `M.unsafeWrite` (y * dctBlockSize + x)) val
                        | y <- [0 .. dctBlockSize - 1]
                        , let blockReadIdx = baseReadIdx + y * w
                        , x <- [0 .. dctBlockSize - 1]
                        , let val = fromIntegral $ src `VS.unsafeIndex` ((blockReadIdx + x) * sampCount + plane)
                        ]
    return block
extractBlock (Image { imageWidth = w, imageHeight = h, imageData = src })
             block sampWidth sampHeight sampCount plane bx by = do
    let accessPixel x y | x < w && y < h = let idx = (y * w + x) * sampCount + plane in src `VS.unsafeIndex` idx
                        | x >= w = accessPixel (w - 1) y
                        | otherwise = accessPixel x (h - 1)

        pixelPerCoeff = fromIntegral $ sampWidth * sampHeight

        blockVal x y = sum [fromIntegral $ accessPixel (xBase + dx) (yBase + dy)
                                | dy <- [0 .. sampHeight - 1]
                                , dx <- [0 .. sampWidth - 1] ] `div` pixelPerCoeff
            where xBase = blockXBegin + x * sampWidth
                  yBase = blockYBegin + y * sampHeight

                  blockXBegin = bx * dctBlockSize * sampWidth
                  blockYBegin = by * dctBlockSize * sampHeight

    sequence_ [(block `M.unsafeWrite` (y * dctBlockSize + x)) $ blockVal x y | y <- [0 .. 7], x <- [0 .. 7] ]
    return block

serializeMacroBlock :: BoolWriteStateRef s
                    -> HuffmanWriterCode -> HuffmanWriterCode
                    -> MutableMacroBlock s Int32
                    -> ST s ()
serializeMacroBlock !st !dcCode !acCode !blk =
 void $ (blk `M.unsafeRead` 0) >>= (fromIntegral >>> encodeDc) >> writeAcs (0, 1)
  where writeAcs acc@(_, 63) =
            void $ (blk `M.unsafeRead` 63) >>= (fromIntegral >>> encodeAcCoefs acc)
        writeAcs acc@(_, i ) =
            (blk `M.unsafeRead`  i) >>= (fromIntegral >>> encodeAcCoefs acc) >>= writeAcs

        encodeDc n = writeBits' st (fromIntegral code) (fromIntegral bitCount)
                        >> when (ssss /= 0) (encodeInt st ssss n)
            where ssss = powerOf $ fromIntegral n
                  (bitCount, code) = dcCode `V.unsafeIndex` fromIntegral ssss

        encodeAc 0         0 = writeBits' st (fromIntegral code) $ fromIntegral bitCount
            where (bitCount, code) = acCode `V.unsafeIndex` 0

        encodeAc zeroCount n | zeroCount >= 16 =
          writeBits' st (fromIntegral code) (fromIntegral bitCount) >>  encodeAc (zeroCount - 16) n
            where (bitCount, code) = acCode `V.unsafeIndex` 0xF0
        encodeAc zeroCount n =
          writeBits' st (fromIntegral code) (fromIntegral bitCount) >> encodeInt st ssss n
            where rrrr = zeroCount `unsafeShiftL` 4
                  ssss = powerOf $ fromIntegral n
                  rrrrssss = rrrr .|. ssss
                  (bitCount, code) = acCode `V.unsafeIndex` fromIntegral rrrrssss

        encodeAcCoefs (            _, 63) 0 = encodeAc 0 0 >> return (0, 64)
        encodeAcCoefs (zeroRunLength,  i) 0 = return (zeroRunLength + 1, i + 1)
        encodeAcCoefs (zeroRunLength,  i) n =
            encodeAc zeroRunLength n >> return (0, i + 1)

encodeMacroBlock :: QuantificationTable
                 -> MutableMacroBlock s Int32
                 -> MutableMacroBlock s Int32
                 -> Int16
                 -> MutableMacroBlock s Int16
                 -> ST s (Int32, MutableMacroBlock s Int32)
encodeMacroBlock quantTableOfComponent workData finalData prev_dc block = do
 -- the inverse level shift is performed internally by the fastDCT routine
 blk <- fastDctLibJpeg workData block
        >>= zigZagReorderForward finalData
        >>= quantize quantTableOfComponent
 dc <- blk `M.unsafeRead` 0
 (blk `M.unsafeWrite` 0) $ dc - fromIntegral prev_dc
 return (dc, blk)

divUpward :: (Integral a) => a -> a -> a
divUpward n dividor = val + (if rest /= 0 then 1 else 0)
    where (val, rest) = n `divMod` dividor

prepareHuffmanTable :: DctComponent -> Word8 -> HuffmanTable
                    -> (JpgHuffmanTableSpec, HuffmanPackedTree)
prepareHuffmanTable classVal dest tableDef =
   (JpgHuffmanTableSpec { huffmanTableClass = classVal
                        , huffmanTableDest  = dest
                        , huffSizes = sizes
                        , huffCodes = V.fromListN 16
                            [VU.fromListN (fromIntegral $ sizes ! i) lst
                                                | (i, lst) <- zip [0..] tableDef ]
                        }, VS.singleton 0)
      where sizes = VU.fromListN 16 $ map (fromIntegral . length) tableDef

-- | Encode an image in jpeg at a reasonnable quality level.
-- If you want better quality or reduced file size, you should
-- use `encodeJpegAtQuality`
encodeJpeg :: Image PixelYCbCr8 -> L.ByteString
encodeJpeg = encodeJpegAtQuality 50

defaultHuffmanTables :: [(JpgHuffmanTableSpec, HuffmanPackedTree)]
defaultHuffmanTables =
    [ prepareHuffmanTable DcComponent 0 defaultDcLumaHuffmanTable
    , prepareHuffmanTable AcComponent 0 defaultAcLumaHuffmanTable
    , prepareHuffmanTable DcComponent 1 defaultDcChromaHuffmanTable
    , prepareHuffmanTable AcComponent 1 defaultAcChromaHuffmanTable
    ]

-- | Function to call to encode an image to jpeg.
-- The quality factor should be between 0 and 100 (100 being
-- the best quality).
encodeJpegAtQuality :: Word8                -- ^ Quality factor
                    -> Image PixelYCbCr8    -- ^ Image to encode
                    -> L.ByteString         -- ^ Encoded JPEG
encodeJpegAtQuality quality img@(Image { imageWidth = w, imageHeight = h }) = encode finalImage
  where finalImage = JpgImage [ JpgQuantTable quantTables
                              , JpgScans JpgBaselineDCTHuffman hdr
                              , JpgHuffmanTable defaultHuffmanTables
                              , JpgScanBlob scanHeader encodedImage
                              ]

        outputComponentCount = 3

        scanHeader = scanHeader'{ scanLength = fromIntegral $ calculateSize scanHeader' }
        scanHeader' = JpgScanHeader
            { scanLength = 0
            , scanComponentCount = outputComponentCount
            , scans = [ JpgScanSpecification { componentSelector = 1
                                             , dcEntropyCodingTable = 0
                                             , acEntropyCodingTable = 0
                                             }
                      , JpgScanSpecification { componentSelector = 2
                                             , dcEntropyCodingTable = 1
                                             , acEntropyCodingTable = 1
                                             }
                      , JpgScanSpecification { componentSelector = 3
                                             , dcEntropyCodingTable = 1
                                             , acEntropyCodingTable = 1
                                             }
                      ]

            , spectralSelection = (0, 63)
            , successiveApproxHigh = 0
            , successiveApproxLow  = 0
            }

        hdr = hdr' { jpgFrameHeaderLength   = fromIntegral $ calculateSize hdr' }
        hdr' = JpgFrameHeader { jpgFrameHeaderLength   = 0
                              , jpgSamplePrecision     = 8
                              , jpgHeight              = fromIntegral h
                              , jpgWidth               = fromIntegral w
                              , jpgImageComponentCount = outputComponentCount
                              , jpgComponents          = [
                                    JpgComponent { componentIdentifier      = 1
                                                 , horizontalSamplingFactor = 2
                                                 , verticalSamplingFactor   = 2
                                                 , quantizationTableDest    = 0
                                                 }
                                  , JpgComponent { componentIdentifier      = 2
                                                 , horizontalSamplingFactor = 1
                                                 , verticalSamplingFactor   = 1
                                                 , quantizationTableDest    = 1
                                                 }
                                  , JpgComponent { componentIdentifier      = 3
                                                 , horizontalSamplingFactor = 1
                                                 , verticalSamplingFactor   = 1
                                                 , quantizationTableDest    = 1
                                                 }
                                  ]
                              }

        lumaQuant = scaleQuantisationMatrix (fromIntegral quality)
                        defaultLumaQuantizationTable
        chromaQuant = scaleQuantisationMatrix (fromIntegral quality)
                            defaultChromaQuantizationTable

        zigzagedLumaQuant = zigZagReorderForwardv lumaQuant
        zigzagedChromaQuant = zigZagReorderForwardv chromaQuant
        quantTables = [ JpgQuantTableSpec { quantPrecision = 0, quantDestination = 0
                                          , quantTable = zigzagedLumaQuant }
                      , JpgQuantTableSpec { quantPrecision = 0, quantDestination = 1
                                          , quantTable = zigzagedChromaQuant }
                      ]

        encodedImage = runST $ do
            let horizontalMetaBlockCount =
                    w `divUpward` (dctBlockSize * maxSampling)
                verticalMetaBlockCount =
                    h `divUpward` (dctBlockSize * maxSampling)
                maxSampling = 2
                lumaSamplingSize = ( maxSampling, maxSampling, zigzagedLumaQuant
                                   , makeInverseTable defaultDcLumaHuffmanTree
                                   , makeInverseTable defaultAcLumaHuffmanTree)
                chromaSamplingSize = ( maxSampling - 1, maxSampling - 1, zigzagedChromaQuant
                                     , makeInverseTable defaultDcChromaHuffmanTree
                                     , makeInverseTable defaultAcChromaHuffmanTree)
                componentDef = [lumaSamplingSize, chromaSamplingSize, chromaSamplingSize]

                imageComponentCount = length componentDef

            dc_table <- M.replicate 3 0
            block <- createEmptyMutableMacroBlock
            workData <- createEmptyMutableMacroBlock
            zigzaged <- createEmptyMutableMacroBlock
            writeState <- newWriteStateRef

            -- It's ugly, I know, be avoid allocation
            let blockDecoder mx my = component $ zip [0..] componentDef
                  where component [] = return ()
                        component ((comp, (sizeX, sizeY, table, dc, ac)) : comp_rest) =
                           rasterMap sizeX sizeY decoder >> component comp_rest
                          where xSamplingFactor = maxSampling - sizeX + 1
                                ySamplingFactor = maxSampling - sizeY + 1
                                extractor = extractBlock img block xSamplingFactor ySamplingFactor imageComponentCount

                                decoder subX subY = do
                                  let blockY = my * sizeY + subY
                                      blockX = mx * sizeX + subX
                                  prev_dc <- dc_table `M.unsafeRead` comp
                                  (dc_coeff, neo_block) <- extractor comp blockX blockY >>=
                                                           encodeMacroBlock table workData zigzaged prev_dc
                                  (dc_table `M.unsafeWrite` comp) $ fromIntegral dc_coeff
                                  serializeMacroBlock writeState dc ac neo_block

            rasterMap 
                horizontalMetaBlockCount verticalMetaBlockCount
                blockDecoder

            finalizeBoolWriter writeState

