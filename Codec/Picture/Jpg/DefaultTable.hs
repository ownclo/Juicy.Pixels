{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Module used by the jpeg decoder internally, shouldn't be used
-- in user code.
module Codec.Picture.Jpg.DefaultTable( DctComponent( .. )
									 , HuffmanTable
									 , QuantificationTable
									 , HuffmanWriterCode 
									 , scaleQuantisationMatrix
									 , makeMacroBlock
									 , makeInverseTable
									 , buildHuffmanTree
									 , packHuffmanTree
									 , huffmanPackedDecode

									 , defaultChromaQuantizationTable

									 , defaultLumaQuantizationTable

									 , defaultAcChromaHuffmanTree
									 , defaultAcChromaHuffmanTable

									 , defaultAcLumaHuffmanTree 
									 , defaultAcLumaHuffmanTable 

									 , defaultDcChromaHuffmanTree 
									 , defaultDcChromaHuffmanTable

                                     , defaultDcLumaHuffmanTree
									 , defaultDcLumaHuffmanTable
									 ) where

import Foreign.Storable ( Storable )
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
import Data.Bits( unsafeShiftL, (.|.), (.&.) )
import Data.Word( Word8, Word16 )

import Codec.Picture.BitWriter
import Codec.Picture.Jpg.Huffman
import Codec.Picture.Jpg.Types( MacroBlock, QuantificationTable )

type HuffmanWriterCode = V.Vector (Word8, Word16)

makeInverseTable :: HuffmanTree -> HuffmanWriterCode
makeInverseTable t = V.replicate 255 (0,0) V.// inner 0 0 t
  where inner _     _     Empty   = []
        inner depth code (Leaf v) = [(fromIntegral v, (depth, code))]
        inner depth code (Branch l r) =
          inner (depth + 1) shifted l ++ inner (depth + 1) (shifted .|. 1) r
            where shifted = code `unsafeShiftL` 1

-- | Helper function to create pure macro block of the good size.
makeMacroBlock :: (Storable a) => [a] -> MacroBlock a
makeMacroBlock = VS.fromListN 64

scaleQuantisationMatrix :: Int -> QuantificationTable -> QuantificationTable 
scaleQuantisationMatrix quality
    | quality < 0 = scaleQuantisationMatrix 0
        -- shouldn't show much difference than with 1,
        -- but hey, at least we're complete
    | quality == 0 = VS.map (scale (10000 :: Int))
    | quality < 50 = let qq = 5000 `div` quality
                     in VS.map (scale qq)
    | otherwise    = VS.map (scale q)
          where q = 200 - quality * 2
                scale coeff i = fromIntegral . min 255 
                                             . max 1 
                                             $ fromIntegral i * coeff `div` 100

huffmanPackedDecode :: HuffmanPackedTree -> BoolReader s Word8
huffmanPackedDecode table = getNextBitJpg >>= aux 0
  where aux idx b
            | (v .&. 0x8000) /= 0 = return  0
            | (v .&. 0x4000) /= 0 = return . fromIntegral $ v .&. 0xFF
            | otherwise = getNextBitJpg >>= aux v
          where tableIndex | b = idx + 1
                           | otherwise = idx
                v = table `VS.unsafeIndex` fromIntegral tableIndex

defaultLumaQuantizationTable :: QuantificationTable
defaultLumaQuantizationTable = makeMacroBlock
    [16, 11, 10, 16,  24,  40,  51,  61
    ,12, 12, 14, 19,  26,  58,  60,  55
    ,14, 13, 16, 24,  40,  57,  69,  56
    ,14, 17, 22, 29,  51,  87,  80,  62
    ,18, 22, 37, 56,  68, 109, 103,  77
    ,24, 35, 55, 64,  81, 104, 113,  92
    ,49, 64, 78, 87, 103, 121, 120, 101
    ,72, 92, 95, 98, 112, 100, 103,  99
    ]

defaultChromaQuantizationTable :: QuantificationTable
defaultChromaQuantizationTable = makeMacroBlock
    [17, 18, 24, 47, 99, 99, 99, 99
    ,18, 21, 26, 66, 99, 99, 99, 99
    ,24, 26, 56, 99, 99, 99, 99, 99
    ,47, 66, 99, 99, 99, 99, 99, 99
    ,99, 99, 99, 99, 99, 99, 99, 99
    ,99, 99, 99, 99, 99, 99, 99, 99
    ,99, 99, 99, 99, 99, 99, 99, 99
    ,99, 99, 99, 99, 99, 99, 99, 99
    ]

defaultDcLumaHuffmanTree :: HuffmanTree
defaultDcLumaHuffmanTree = buildHuffmanTree defaultDcLumaHuffmanTable

-- | From the Table K.3 of ITU-81 (p153)
defaultDcLumaHuffmanTable :: HuffmanTable
defaultDcLumaHuffmanTable =
    [ []
    , [0]
    , [1, 2, 3, 4, 5]
    , [6]
    , [7]
    , [8]
    , [9]
    , [10]
    , [11]
    , []
    , []
    , []
    , []
    , []
    , []
    , []
    ]

defaultDcChromaHuffmanTree :: HuffmanTree
defaultDcChromaHuffmanTree = buildHuffmanTree defaultDcChromaHuffmanTable

-- | From the Table K.4 of ITU-81 (p153)
defaultDcChromaHuffmanTable :: HuffmanTable
defaultDcChromaHuffmanTable = 
    [ []
    , [0, 1, 2]
    , [3]
    , [4]
    , [5]
    , [6]
    , [7]
    , [8]
    , [9]
    , [10]
    , [11]
    , []
    , []
    , []
    , []
    , []
    ]

defaultAcLumaHuffmanTree :: HuffmanTree
defaultAcLumaHuffmanTree = buildHuffmanTree defaultAcLumaHuffmanTable

-- | From the Table K.5 of ITU-81 (p154)
defaultAcLumaHuffmanTable :: HuffmanTable
defaultAcLumaHuffmanTable =
    [ []
    , [0x01, 0x02]
    , [0x03]
    , [0x00, 0x04, 0x11]
    , [0x05, 0x12, 0x21]
    , [0x31, 0x41]
    , [0x06, 0x13, 0x51, 0x61]
    , [0x07, 0x22, 0x71]
    , [0x14, 0x32, 0x81, 0x91, 0xA1]
    , [0x08, 0x23, 0x42, 0xB1, 0xC1]
    , [0x15, 0x52, 0xD1, 0xF0]
    , [0x24, 0x33, 0x62, 0x72]
    , []
    , []
    , [0x82]
    , [0x09, 0x0A, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2A, 0x34, 0x35
      ,0x36, 0x37, 0x38, 0x39, 0x3A, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4A, 0x53, 0x54
      ,0x55, 0x56, 0x57, 0x58, 0x59, 0x5A, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x73
      ,0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7A, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8A
      ,0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9A, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7
      ,0xA8, 0xA9, 0xAA, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xC2, 0xC3, 0xC4
      ,0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9, 0xDA
      ,0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9, 0xEA, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5
      ,0xF6, 0xF7, 0xF8, 0xF9, 0xFA]
    ]

type HuffmanTable = [[Word8]]

defaultAcChromaHuffmanTree :: HuffmanTree
defaultAcChromaHuffmanTree = buildHuffmanTree defaultAcChromaHuffmanTable 

defaultAcChromaHuffmanTable :: HuffmanTable
defaultAcChromaHuffmanTable = 
    [ []
    , [0x00, 0x01]
    , [0x02]
    , [0x03, 0x11]
    , [0x04, 0x05, 0x21, 0x31]
    , [0x06, 0x12, 0x41, 0x51]
    , [0x07, 0x61, 0x71]
    , [0x13, 0x22, 0x32, 0x81]
    , [0x08, 0x14, 0x42, 0x91, 0xA1, 0xB1, 0xC1]
    , [0x09, 0x23, 0x33, 0x52, 0xF0]
    , [0x15, 0x62, 0x72, 0xD1]
    , [0x0A, 0x16, 0x24, 0x34]
    , []
    , [0xE1]
    , [0x25, 0xF1]
    , [ 0x17, 0x18, 0x19, 0x1A, 0x26, 0x27, 0x28, 0x29, 0x2A, 0x35
      , 0x36, 0x37, 0x38, 0x39, 0x3A, 0x43, 0x44, 0x45, 0x46, 0x47
      , 0x48, 0x49, 0x4A, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59
      , 0x5A, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x73
      , 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7A, 0x82, 0x83, 0x84
      , 0x85, 0x86, 0x87, 0x88, 0x89, 0x8A, 0x92, 0x93, 0x94, 0x95
      , 0x96, 0x97, 0x98, 0x99, 0x9A, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6
      , 0xA7, 0xA8, 0xA9, 0xAA, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7
      , 0xB8, 0xB9, 0xBA, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8
      , 0xC9, 0xCA, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9
      , 0xDA, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9, 0xEA
      , 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA
      ]
    ]

