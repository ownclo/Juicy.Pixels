{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module Codec.Picture.Jpg.Env where

import Data.Bits( Bits, unsafeShiftL, (.|.) )
import Data.Maybe(fromJust)
import Data.Word(Word8, Word16)
import qualified Data.Map as M
import Control.Lens(makeLenses)
import Data.Int(Int16)

type Byte = Word8
type Word = Word16

type Table a = M.Map Int a
type BC = Int
type Run = Int

data HClass = ACHuff | DCHuff deriving Show

type QTable = [Int16]


---------------------------
---- MISC FUNCTIONS -------
---------------------------
byte2nibs :: (Integral a) => a -> (a, a)
byte2nibs = (`divMod` 16)

nibs2byte :: (Bits a) => (a, a) -> a
nibs2byte (a, b) = (a `unsafeShiftL` 4) .|. b

fI :: (Integral a, Num b) => a -> b
fI = fromIntegral

hClassFromIndex :: Byte -> HClass
hClassFromIndex 0 = DCHuff
hClassFromIndex _ = ACHuff

indexFromHClass :: HClass -> Byte
indexFromHClass DCHuff = 0
indexFromHClass ACHuff = 1


-- supported markers
data Marker = SOI -- start of input
            | EOI -- end of input
            | SOF -- start of frame
            | SOS -- start of scan
            | DQT -- define quantization table
            | DHT -- define huffman table
    deriving (Show, Eq)

markerCodes :: [(Marker, Word8)]
markerCodes =  [(SOI, 0xD8)
               ,(EOI, 0xD9)
               ,(SOF, 0xC0)
               ,(SOS, 0xDA)
               ,(DQT, 0xDB)
               ,(DHT, 0xC4)]

knownMarkers :: [Word8]
knownMarkers = map snd markerCodes

-- fromJust is either safe, or it is pointless to continue.
markerCode :: Marker -> Word8
markerCode = fromJust . (`lookup` markerCodes) -- that's safe, I guarantee it.


data Dim a = Dim {
           _y, _x :: !a
    } deriving (Show, Functor)

toDim :: (a, a) -> Dim a
toDim (!y, !x) = Dim y x

fromDim :: Dim a -> (a, a)
fromDim (Dim !y !x) = (y, x)

getY, getX :: Dim a -> a
getY (Dim y _) = y
getX (Dim _ x) = x


data QTableSpec = QTableSpec {
                _id :: !Int,
                _precision :: !Byte,
                _qTable :: QTable
    } deriving (Show)


data FrameCompSpec = FrameCompSpec {
            _compId :: {-# UNPACK #-} !Byte, -- component identifier
            _sampF  :: Dim Byte,             -- sampling factors
            _qTabI  :: {-# UNPACK #-} !Byte  -- quantization table index
    } deriving Show

data FrameHeader = FrameHeader {
            _size :: Dim Word, -- size of an image
            _fcs  :: Table FrameCompSpec
    } deriving Show

data ScanCompSpec = ScanCompSpec {
            _scId :: {-# UNPACK #-} !Byte, -- component identifier (within a scan)
            _dcId :: {-# UNPACK #-} !Byte, -- DC table to be used within component
            _acId :: {-# UNPACK #-} !Byte  -- AC table to be used within component
    } deriving Show

type ScanHeader = [ScanCompSpec]

data HuffmanSegment = HFS {
            _type    :: !HClass,              -- AC or DC table
            _tableId :: {-# UNPACK #-} !Byte, -- id of a table (see _dcId)
            _lengths :: [Int],
            _values  :: [[Word8]]
    } deriving Show

data HuffTables = HuffTables { -- retrieves a huffman tree given its type and id.
            _dcTable :: Table HuffmanSegment,
            _acTable :: Table HuffmanSegment
    } deriving Show

-- Environment will be updated by headers.
data Env = Env {
            _huffTables  :: HuffTables,       -- added by DHT (HuffmanSegment)
            _qTables     :: Table QTableSpec, -- added by DQT
            _frameHeader :: FrameHeader,      -- added by SOF
            _scanHeader  :: ScanHeader        -- added by SOS
    } deriving Show

makeLenses ''Env
makeLenses ''FrameHeader
makeLenses ''HuffTables
