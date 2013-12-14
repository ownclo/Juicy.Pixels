{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Codec.Picture.Jpg.Printer
    ( printHeader
    ) where

import Codec.Picture.Jpg.Env

import Control.Applicative( (<$>) )
import Control.Monad( forM_ )
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L
import Data.Word( Word8, Word16 )
import Data.Binary.Put( runPut
                      , Put
                      , putWord8
                      , putWord16be
                      , putLazyByteString
                      )

type LS = L.ByteString
type Printer a = a -> Put

--- PRIMITIVE PRINTERS ---
byte :: Printer Byte
byte = putWord8

word :: Printer Word
word = putWord16be

byteI :: Printer Int
byteI = byte . fI

wordI :: Printer Int
wordI = word . fI

nibbles :: Printer (Byte, Byte)
nibbles = byte . nibs2byte

marker :: Printer Marker
marker m = do
        byte 0xFF
        byte $ markerCode m


--- SEGMENT PRINTERS ---
type TableList a = [a]
data QTableSpec = QTableSpec {
                _id :: !Int,
                _precision :: !Byte,
                _qTable :: QTable
    }

class SizeCalculable a where
    calculateSize :: a -> Int

instance SizeCalculable QTableSpec where
        calculateSize (QTableSpec{_precision = p}) = 1 + 64 * coeff
            where coeff = if p == 0 then 1 else 2

instance SizeCalculable (TableList QTableSpec) where
        calculateSize tlist = 2 + sum (calculateSize <$> tlist)

instance SizeCalculable FrameHeader where
        calculateSize (FrameHeader size fcspecs) = 2 + 1 + 2 + 2 + 1 +
            M.size fcspecs * 3

instance SizeCalculable ScanHeader where
        calculateSize sh = 2 + 1 + (length sh * 3) + 1 + 1 + 1

instance SizeCalculable HuffmanSegment where
        calculateSize (HFS{_lengths = ls}) = 1 + 16 + sum ls

quanTable :: Printer QTableSpec
quanTable (QTableSpec id p qTable) = do
        nibbles (p, fI id)
        let coeff = if p == 0 then byteI else wordI
        forM_ (fI <$> qTable) coeff

quanTablesSegment :: Printer (TableList QTableSpec)
quanTablesSegment segment = do
        marker DQT
        wordI $ calculateSize segment
        sequence_ $ quanTable <$> segment

frameCompSpec :: Printer FrameCompSpec
frameCompSpec (FrameCompSpec id sf tq) = do
        byte id
        nibbles $ fromDim sf
        byte tq

startOfFrame :: Printer FrameHeader
startOfFrame fh@(FrameHeader (Dim y x) fcspecs) = do
        marker SOF
        wordI $ calculateSize fh
        byte 0 -- Sample precision. Unsupported (0 means 8bit)
        word y
        word x
        byteI $ M.size fcspecs
        let fclist = map snd $ M.toList fcspecs
        forM_ fclist frameCompSpec

scanCompSpec :: Printer ScanCompSpec
scanCompSpec (ScanCompSpec cs dct act) = do
        byte cs
        nibbles (dct, act)

startOfScan :: Printer ScanHeader
startOfScan scanHeader = do
        marker SOS
        wordI $ calculateSize scanHeader
        byteI $ length scanHeader
        forM_ scanHeader scanCompSpec
        byte 0
        byte 63
        nibbles (0, 0) -- approximation parameter. unused in sequential mode

huffTableSegment :: Printer HuffmanSegment
huffTableSegment hfs@(HFS hClass id lengths values) = do
        marker DHT
        wordI $ calculateSize hfs
        nibbles (indexFromHClass hClass, id)
        forM_ lengths byteI
        forM_ values $ mapM_ byte


--- ENVIRONMENT PRINTERS ---
-- quanTables :: Printer Env
-- quanTables (Env{_qTables=qs}) = do
--         forM_ qs quanTablesSegment

printEnv :: Printer Env
printEnv = undefined


printHeader :: Env -> LS
printHeader = runPut . printEnv
