module Codec.Picture.Jpg.EnvReader
    ( ImageSpec(..)
    , CompMCUSpec(..)
    , DataUnitSpec(..)
    , getImageSpec
    ) where

import Codec.Picture.Jpg.Env
import Codec.Picture.Jpg.Huffman( packHuffmanTree, HuffmanPackedTree )
import Codec.Picture.Jpg.Types( MacroBlock )

import Control.Applicative

import qualified Data.Map as M
import Data.Maybe(fromJust)
import qualified Data.Vector.Storable as VS
import Data.Int( Int16 )

data ImageSpec = ImageSpec {
            compCount_  :: !Int,
            imgSize_    :: !(Int, Int),
            imgMcuSize_ :: !(Int, Int),
            maxSFacts_  :: !(Int, Int),
            mcuSpec_    :: [CompMCUSpec]
    } deriving Show

data CompMCUSpec = CompMCUSpec {
            _compIdx      :: !Int,
            _numDataUnits :: !(Int, Int), -- number of DUs in an MCU (h and v)
            _scaleFactors :: !(Int, Int), -- miltiplicators of _one_ DU
            _duSpec       :: !DataUnitSpec
    } deriving Show

-- Everything that is needed in order to
-- decode particular Data Unit.
data DataUnitSpec = DataUnitSpec {
            _qTable :: !(MacroBlock Int16),
            _dcTree :: !HuffmanPackedTree,
            _acTree :: !HuffmanPackedTree
    } deriving Show

data FullCompSpec = FullCompSpec {
            _fs :: !FrameCompSpec,
            _ss :: !ScanCompSpec
    } deriving Show

getImageSpec :: Env -> ImageSpec
getImageSpec (Env (HuffTables dcT acT)
                  quanT
                  (FrameHeader (Dim y x) frameCS)
                  scanH) = imageSpec where

    imageSpec = ImageSpec compCount
                          (imgWidth, imgHeight)
                          (imgMcuWidth, imgMcuHeight)
                          (maxXsf, maxYsf)
                          mcuSpec

    compCount = length scanH
    imgWidth = fI x
    imgHeight = fI y
    imgMcuWidth = imgWidth `ceilDiv` (8 * maxXsf)
    imgMcuHeight = imgHeight `ceilDiv` (8 * maxYsf)
    (maxYsf, maxXsf) = getMaxSampFactors fullCompSpecs
    mcuSpec = zipWith buildCompMCU [0..] fullCompSpecs

    fullCompSpecs = zipById frameCS scanH

    upSampFactor (Dim h w) = (maxYsf `div` h, maxXsf `div` w)

    buildCompMCU compIdx (FullCompSpec
                            (FrameCompSpec _ samplings qI)
                            (ScanCompSpec _ dcI acI))
                = compMCUSpec where
        compMCUSpec = CompMCUSpec compIdx
                                  (duWidth, duHeight)
                                  (subX, subY)
                                  duSpec

        nd@(Dim duHeight duWidth) = fI <$> samplings
        (subY, subX) = upSampFactor nd
        duSpec = DataUnitSpec qtable dctree actree

        qtable = VS.fromListN 64 . fromJust $ M.lookup (fI qI) quanT
        dctree = packHuffmanTree . fromJust $ M.lookup (fI dcI) dcT
        actree = packHuffmanTree . fromJust $ M.lookup (fI acI) acT

getMaxSampFactors :: [FullCompSpec] -> (Int, Int)
getMaxSampFactors fullCompSpecs = (maxYsf, maxXsf) where
    maxYsf = fI . maximum $! map (getY . getSF) fullCompSpecs
    maxXsf = fI . maximum $! map (getX . getSF) fullCompSpecs
    getSF (FullCompSpec (FrameCompSpec _ sf _) _) = sf

zipById :: Table FrameCompSpec -> [ScanCompSpec] -> [FullCompSpec]
zipById tfcs = map addFcs where
    addFcs sc = case M.lookup (fromIntegral $ _scId sc) tfcs of
                    Nothing -> error "Frame header corrupted. Aborting."
                    Just fc -> FullCompSpec fc sc

-- helpers --
ceilDiv :: Int -> Int -> Int
ceilDiv n d = (n+d-1)`div`d
