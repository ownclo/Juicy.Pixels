{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
module Codec.Picture.Jpg.Printer
    ( printImage
    ) where

import Codec.Picture.Jpg.Env

import Control.Applicative( (<$>) )
import Control.Monad( forM_ )
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L
import Data.Binary.Put( runPut
                      , Put
                      , putWord8
                      , putWord16be
                      , putLazyByteString
                      )

type LS = L.ByteString
type Printer a = a -> Put

toList :: M.Map a b -> [b]
toList = map snd . M.toList

--- PRIMITIVE PRINTERS ---
printRaw :: Printer LS
printRaw = putLazyByteString

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
type TableList a = [a] -- juggling with type system.

class SizeCalculable a where
    calculateSize :: a -> Int

instance SizeCalculable QTableSpec where
        calculateSize (QTableSpec{_precision = p}) = 1 + 64 * coeff
            where coeff = if p == 0 then 1 else 2

instance SizeCalculable (TableList QTableSpec) where
        calculateSize tlist = 2 + sum (calculateSize <$> tlist)

instance SizeCalculable FrameHeader where
        calculateSize (FrameHeader _ fcspecs) = 2 + 1 + 2 + 2 + 1 +
            M.size fcspecs * 3

instance SizeCalculable ScanHeader where
        calculateSize sh = 2 + 1 + (length sh * 3) + 1 + 1 + 1

instance SizeCalculable HuffmanSegment where
        calculateSize (HFS{_lengths = ls}) = 1 + 16 + sum ls

quanTable :: Printer QTableSpec
quanTable (QTableSpec id' p qTable) = do
        nibbles (p, fI id')
        let coeff = if p == 0 then byteI else wordI
        mapM_ coeff $ fI <$> qTable

quanTablesSegment :: Printer (TableList QTableSpec)
quanTablesSegment segment = do
        marker DQT
        wordI $ calculateSize segment
        sequence_ $ quanTable <$> segment

frameCompSpec :: Printer FrameCompSpec
frameCompSpec (FrameCompSpec id' sf tq) = do
        byte id'
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
        mapM_ frameCompSpec $ toList fcspecs

scanCompSpec :: Printer ScanCompSpec
scanCompSpec (ScanCompSpec cs dct act) = do
        byte cs
        nibbles (dct, act)

startOfScan :: Printer ScanHeader
startOfScan sHeader = do
        marker SOS
        wordI $ calculateSize sHeader
        byteI $ length sHeader
        mapM_ scanCompSpec sHeader
        byte 0  -- magic numbers. unused in sequential mode
        byte 63
        nibbles (0, 0)

huffTableSegment :: Printer HuffmanSegment
huffTableSegment hfs@(HFS hClass id' lengths values) = do
        marker DHT
        wordI $ calculateSize hfs
        nibbles (indexFromHClass hClass, id')
        mapM_ byteI lengths
        forM_ values $ mapM_ byte


--- ENVIRONMENT PRINTERS ---
quanTables :: Printer Env
quanTables (Env{_qTables=qtables}) = quanTablesSegment $ toList qtables

huffTable :: Printer Env
huffTable (Env{_huffTables=(HuffTables dcT acT)}) = do
        mapM_ huffTableSegment $ toList dcT
        mapM_ huffTableSegment $ toList acT

frameDesc :: Printer Env
frameDesc (Env{_frameHeader=fh}) = startOfFrame fh

scanDesc :: Printer Env
scanDesc (Env{_scanHeader=sh}) = startOfScan sh

printEnv :: Printer Env
printEnv env = do
        quanTables env
        frameDesc  env
        huffTable  env
        scanDesc   env

printImage :: Env -> LS -> LS
printImage !env contents = runPut $ do
        marker SOI
        printEnv env
        printRaw contents
        marker EOI
