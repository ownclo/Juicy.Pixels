module Codec.Picture.Jpg.Parser
    ( parseHeader
    ) where

import Codec.Picture.Jpg.Env
import Codec.Picture.Jpg.Huffman(buildHuffmanTree)

import Control.Applicative
import Control.Monad.State
import Prelude hiding(take, id)

import Control.Lens((.=), (%=))
-- l .= v -> substitute state with v;
-- l %= f -> modify state with f

import Data.Attoparsec.Number ()
import Data.Attoparsec
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import Data.Word( Word8 )

type BS = B8.ByteString

---------------------------
---- PRIMITIVE PARSERS ----
---------------------------
theByte :: Word8 -> Parser ()
theByte = void . word8

byteI :: Parser Int
byteI = fI <$> anyWord8

byte :: Parser Byte
byte = fI <$> byteI

word :: Parser Word
word = do
        a <- byte
        b <- byte
        return $ to16 a * 256 + to16 b
        where to16 a = fI a :: Word

wordI :: Parser Int
wordI = do
        a <- byteI
        b <- byteI
        return $ a * 256 + b

nibbles :: Parser (Byte, Byte)
nibbles = liftM byte2nibs byte

marker :: Marker -> Parser ()
marker m = void $ do
        theByte 0xFF
        theByte $ markerCode m

getMarker :: Parser Word8
getMarker = theByte 0xFF >> anyWord8


---------------------------
----- SEGMENT PARSERS -----
---------------------------
unknownSegment :: Parser ()
unknownSegment = do
        mark <- getMarker
        guard (mark `notElem` knownMarkers)
        len  <- wordI
        void . take $ len - 2 -- the length of 'len' (Word16) itself is included.

quanTable :: Parser (Int, QTableSpec)
quanTable = do
        (p, id) <- nibbles
        qTable  <- 64 `count` (if p==0 then byteI else wordI)
        let id' = fI id
            qspec = QTableSpec id' p (map fI qTable)
        return (id', qspec)

-- NOTE: QTable consists of 64 quantization parameters.
-- A QTable can be of 1- or 2-byte precision. Yet another byte
-- precedes q-values. That byte indicates the type of precision
-- and the index of QTable (see quanTable). So, the size
-- of each table is 1 + (1|2)*64. How can we deduce the number
-- of QTables given their accumulative length? As it can be
-- seen, the number of extra bytes indicates this properly.
--      len = k * (1 + (1|2)*64)
--      len mod 64 = k mod 64 + k*(1|2)*64 mod 64
--      len mod 64 = k mod 64
--      len mod 64 = k // see below.
-- The last equation holds iff k < 64. We assume that this is
-- the case for any image (not necessarily, but more than likely),
-- because the index space of QTables is of 4 bit size.
quanTablesSegment :: Parser [(Int, QTableSpec)]
quanTablesSegment = do
        marker DQT
        len <- wordI
        let n = (len - 2) `rem` 64 -- WHY? See the comment above.
        n `count` quanTable

frameCompSpec :: Parser (Int, FrameCompSpec)
frameCompSpec = do
        id <- byte
        sf <- nibbles
        tq <- byte
        return (fI id, FrameCompSpec id (toDim sf) tq)

startOfFrame :: Parser FrameHeader
startOfFrame = do
        marker SOF
        _ <- word  -- Length. Assuming that length does match.
        _ <- byte  -- Sample precision. Unsupported (always byte)
        y <- word  -- Vertical size of a picture (in pixels)
        x <- word  -- Horizonal size of a picture (in pixels)
        n <- byteI -- Number of color components.
        fcspecs <- n `count` frameCompSpec
        return $ FrameHeader (toDim (y, x)) (M.fromList fcspecs)

scanCompSpec :: Parser ScanCompSpec
scanCompSpec = do
        cs <- byte
        (td,ta) <- nibbles
        return $ ScanCompSpec cs td ta

startOfScan :: Parser ScanHeader
startOfScan = do
        marker SOS
        _   <- word    -- length, unused
        n   <- fI <$> byte
        scs <- n `count` scanCompSpec
        0   <- byte    -- ss, select spectral predictor. 0 for lossless mode.
        63  <- byte    -- se, end of spectral predictor. always set to 63
        _   <- nibbles -- approximation parameter. unused in sequential mode.
        return scs

huffTableSegment :: Parser HuffmanSegment
huffTableSegment = do
        marker DHT
        _ <- word              -- length, unused
        (tc, th) <- nibbles    -- table class (ac | dc), table header (aka id)
        ls <- 16 `count` byteI -- number of huffman codes with given length
        values <- mapM (`count` byte) ls

        let hClass = hClassFromIndex tc
            id = fI th -- that was a bad name

        return $ HFS hClass id ls values


---------------------------
--- ENVIRONMENT PARSERS ---
---------------------------

-- NOTE: The order is relevant!
-- 'new `union` old' OVERRIDES duplicated keys from old to new,
-- whereas 'old `union` new' doesn't. We need the first.
quanTables :: EnvParser ()
quanTables = do
        tables <- M.fromList <$> lift quanTablesSegment
        qTables %= M.union tables

huffTable :: EnvParser ()
huffTable = do
        hfs@(HFS hClass id' _ values) <- lift huffTableSegment
        let id = fI id'
        case hClass of
             DCHuff -> huffTables.dcTable %= M.insert id hfs
             ACHuff -> huffTables.acTable %= M.insert id hfs

frameDesc :: EnvParser ()
frameDesc = (frameHeader .=) =<< lift startOfFrame

scanDesc :: EnvParser ()
scanDesc = (scanHeader .=) =<< lift startOfScan

markerSegments :: [EnvParser()]
markerSegments = [quanTables, huffTable, lift unknownSegment]

tablesMisc :: EnvParser ()
tablesMisc = void . many $ eitherOf markerSegments
    where
        eitherOf :: (Alternative f) => [f a] -> f a
        eitherOf = foldl1 (<|>)

jpegHeader :: EnvParser ()
jpegHeader = do
        lift $ marker SOI
        tablesMisc >> frameDesc
        tablesMisc >> scanDesc


--- ENVIRONMENT MANAGEMENT ---
type EnvParser = StateT Env Parser

parseEnv :: EnvParser a -> BS -> Either String (Env, BS)
parseEnv f = toEither `o` parse $ runStateT f initialEnv
    where toEither (Fail{})    = Left "Header parsing failure: unsupported format"
          toEither (Partial _) = Left "Header parsing failure: partial result"
          toEither (Done r (_, env)) = Right (env, r)

          o = (.).(.) -- (g `o` h) x y = g (h x y)

          initialEnv = Env {
               _huffTables  = HuffTables emptyTable emptyTable
              ,_qTables     = emptyTable
              ,_frameHeader = error "No frame header found"
              ,_scanHeader  = error "No scan header found"}
          emptyTable = M.empty

parseHeader :: BS -> Either String (Env, BS)
parseHeader = parseEnv jpegHeader
