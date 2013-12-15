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

import Control.Applicative( (<$>) )
import Control.Monad( when, forM_, void )
import Control.Monad.ST( ST, runST )
import Control.Monad.Trans( lift )

import Data.Bits( (.|.), unsafeShiftL )
import Data.Int( Int16, Int32 )
import Data.Word(Word8, Word32)
import qualified Data.Map as M

import qualified Data.Vector as V
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
import Codec.Picture.Jpg.Printer
import Codec.Picture.Jpg.Env
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
 void $ (blk `M.unsafeRead` 0) >>= encodeDc >> writeAcs (0, 1)
  where writeAcs acc@(_, 63) = void $ (blk `M.unsafeRead` 63) >>= encodeAcCoefs acc
        writeAcs acc@(_, i ) = (blk `M.unsafeRead`  i) >>= encodeAcCoefs acc >>= writeAcs

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

-- | Encode an image in jpeg at a reasonnable quality level.
-- If you want better quality or reduced file size, you should
-- use `encodeJpegAtQuality`
encodeJpeg :: Image PixelYCbCr8 -> L.ByteString
encodeJpeg = encodeJpegAtQuality 50

defaultHuffTables :: HuffTables
defaultHuffTables = HuffTables {
    _dcTable = M.fromList
        [(0, HFS {_type = DCHuff
                 ,_tableId = 0
                 ,_lengths = map length defaultDcLumaHuffmanTable
                 ,_values = defaultDcLumaHuffmanTable
                 })
        ,(1, HFS {_type = DCHuff
                 ,_tableId = 1
                 ,_lengths = map length defaultDcChromaHuffmanTable
                 ,_values = defaultDcChromaHuffmanTable
                 })
        ],
    _acTable = M.fromList
        [(0, HFS {_type = ACHuff
                 ,_tableId = 0
                 ,_lengths = map length defaultAcLumaHuffmanTable
                 ,_values = defaultAcLumaHuffmanTable
                 })
        ,(1, HFS {_type = ACHuff
                 ,_tableId = 1
                 ,_lengths = map length defaultAcChromaHuffmanTable
                 ,_values = defaultAcChromaHuffmanTable
                 })
        ]
    }

defaultFrameCompSpec :: Table FrameCompSpec
defaultFrameCompSpec = M.fromList
    [(1, FrameCompSpec {_compId = 1
                       ,_sampF  = Dim 2 2
                       ,_qTabI  = 0
                       })
    ,(2, FrameCompSpec {_compId = 2
                       ,_sampF  = Dim 1 1
                       ,_qTabI  = 1
                       })
    ,(3, FrameCompSpec {_compId = 3
                       ,_sampF  = Dim 1 1
                       ,_qTabI  = 1
                       })
    ]

defaultScanCompSpecs :: [ScanCompSpec]
defaultScanCompSpecs =
    [ScanCompSpec {_scId = 1
                  ,_dcId = 0
                  ,_acId = 0
                  }
    ,ScanCompSpec {_scId = 2
                  ,_dcId = 1
                  ,_acId = 1
                  }
    ,ScanCompSpec {_scId = 3
                  ,_dcId = 1
                  ,_acId = 1
                  }
    ]

encodeJpegAtQuality :: Word8
                    -> Image PixelYCbCr8
                    -> L.ByteString
encodeJpegAtQuality quality
                    img@(Image {imageWidth  = w,
                                imageHeight = h}) = printImage env content
  where env = Env {_huffTables = defaultHuffTables,
                   _qTables = defaultQuantTables,
                   _frameHeader = FrameHeader {_size = Dim (fI h) (fI w),
                                               _fcs = defaultFrameCompSpec},
                   _scanHeader = defaultScanCompSpecs}

        lumaQuant = scaleQuantisationMatrix (fromIntegral quality)
                        defaultLumaQuantizationTable
        chromaQuant = scaleQuantisationMatrix (fromIntegral quality)
                        defaultChromaQuantizationTable

        zigzagedLumaQuant = zigZagReorderForwardv lumaQuant
        zigzagedChromaQuant = zigZagReorderForwardv chromaQuant

        defaultQuantTables = M.fromList
            [(0, QTableSpec {_id = 0
                            ,_precision = 0
                            ,_qTable = VS.toList zigzagedLumaQuant
                            })
            ,(1, QTableSpec {_id = 1
                            ,_precision = 0
                            ,_qTable = VS.toList zigzagedChromaQuant
                            })
            ]

        content = runST $ do
            let mcuWidth  = w `ceilDiv` (dctBlockSize * maxSampling)
                mcuHeight = h `ceilDiv` (dctBlockSize * maxSampling)
                maxSampling = 2
                lumaMCUSpec   = ( maxSampling, maxSampling, zigzagedLumaQuant
                                , makeInverseTable defaultDcLumaHuffmanTree
                                , makeInverseTable defaultAcLumaHuffmanTree )
                chromaMCUSpec = ( maxSampling - 1, maxSampling - 1, zigzagedChromaQuant
                                , makeInverseTable defaultDcChromaHuffmanTree
                                , makeInverseTable defaultAcChromaHuffmanTree )

                mcuSpec = zip [0..] [lumaMCUSpec, chromaMCUSpec, chromaMCUSpec]
                numComp = length mcuSpec -- 3

            dcCoeffs <- M.replicate numComp 0
            block    <- createEmptyMutableMacroBlock
            workData <- createEmptyMutableMacroBlock
            zigzaged <- createEmptyMutableMacroBlock
            writeState <- newWriteStateRef

            let mcuEncoder mx my = compMcuEncoder mcuSpec
                  where compMcuEncoder [] = return ()
                        compMcuEncoder ((cIdx, (sampX, sampY, qTable, dcT, acT)) : rest) =
                            rasterMap sampX sampY blockEncoder >> compMcuEncoder rest
                          where xSamp = maxSampling - sampX + 1
                                ySamp = maxSampling - sampY + 1
                                extractor = extractBlock img block xSamp ySamp numComp

                                blockEncoder subX subY = do
                                    let blockY = my * sampY + subY
                                        blockX = mx * sampX + subX
                                    prevDc <- dcCoeffs `M.unsafeRead` cIdx
                                    (newDc, newBlock) <- extractor cIdx blockX blockY >>=
                                                         encodeMacroBlock qTable workData zigzaged prevDc
                                    dcCoeffs `M.unsafeWrite` cIdx $ fI newDc
                                    serializeMacroBlock writeState dcT acT newBlock

            rasterMap mcuWidth mcuHeight mcuEncoder
            finalizeBoolWriter writeState
