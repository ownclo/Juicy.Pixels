{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module Codec.Picture.Jpg.Env where

import Codec.Picture.Jpg.Huffman(HuffmanTree)

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

type QTable = [Int16] --- XXX: Temporary!

data Dim a = Dim {
           _y, _x :: !a
    } deriving (Show, Functor)

toDim :: (a, a) -> Dim a
toDim (!y, !x) = Dim y x

getY, getX :: Dim a -> a
getY (Dim y _) = y
getX (Dim _ x) = x


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
            _tree    :: HuffmanTree
    } deriving Show

data HuffTables = HuffTables { -- retrieves a huffman tree given its type and id.
            _dcTable :: Table HuffmanTree,
            _acTable :: Table HuffmanTree
    } deriving Show

-- Environment will be updated by headers.
data Env = Env {
            _huffTables  :: HuffTables,   -- added by DHT (HuffmanSegment)
            _qTables     :: Table QTable, -- added by DQT
            _frameHeader :: FrameHeader,  -- added by SOF
            _scanHeader  :: ScanHeader    -- added by SOS
    } deriving Show

makeLenses ''Env
makeLenses ''FrameHeader
makeLenses ''HuffTables