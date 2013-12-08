{-# LANGUAGE BangPatterns #-}
module Codec.Picture.Jpg.EnvReader
    ( getMCUSpec
    ) where

import Codec.Picture.Jpg.Env

import Control.Applicative

import qualified Data.Map as M
import Data.Maybe(fromJust)

data FullCompSpec = FullCompSpec {
            _fs :: !FrameCompSpec,
            _ss :: !ScanCompSpec
    } deriving Show

data CompMCUSpec = CompMCUSpec {
            _numDataUnits :: !(Dim Int), -- number of DUs in an MCU (h and v)
            _duSpec       :: !DataUnitSpec
    } deriving Show

type MCUSpec = [CompMCUSpec]

-- Everything that is needed in order to
-- decode particular Data Unit.
data DataUnitSpec = DataUnitSpec {
            _scaleFactors :: !(Dim Int), -- miltiplicators of _one_ DU
            _qTable :: !QTable,
            _dcTree :: !DCHuffTree,
            _acTree :: !ACHuffTree
    } deriving Show

getMCUSpec :: Env -> (Dim Int, MCUSpec)
getMCUSpec (Env (HuffTables dcT acT)
                quanT
                (FrameHeader (Dim x y) frameCS)
                scanH) = (numMCUs, mcuSpec) where

    fullCompSpecs = zipById frameCS scanH
    (maxYsf, maxXsf) = getMaxSampFactors fullCompSpecs
    mcuSpec = map buildCompMCU fullCompSpecs

    numMCUs = Dim (fI y `ceilDiv` 8*maxYsf)
                  (fI x `ceilDiv` 8*maxXsf)

    upSampFactor (Dim h w) = Dim (maxYsf `div` h)
                                 (maxXsf `div` w)

    -- it needs all tables + maxY and maxX from outer closure.
    buildCompMCU (FullCompSpec
                    (FrameCompSpec _ samplings qI)
                    (ScanCompSpec _ dcI acI))
                = CompMCUSpec nd duSpec where
        nd = fI <$> samplings
        ups = upSampFactor nd
        duSpec = DataUnitSpec ups qtable dctree actree

        qtable = fromJust $ M.lookup (fI qI) quanT
        dctree = fromJust $ M.lookup (fI dcI) dcT
        actree = fromJust $ M.lookup (fI acI) acT

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

fI :: (Integral a, Num b) => a -> b
fI = fromIntegral
