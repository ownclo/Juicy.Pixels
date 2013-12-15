{-# LANGUAGE DoAndIfThenElse #-}

module Codec.Picture.Jpg.Huffman
    ( HuffmanTree(..) -- XXX: Do we need to isolate inner constructors?
    , DctComponent(..)
    , HuffmanPackedTree
    , prepareHuffTree
    , packHuffmanTree
    , buildHuffmanTree
    ) where

import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as M
import Data.Bits( (.|.) )
import Control.Monad.ST( runST )

import Control.Monad.State.Strict
import Data.Word( Word8, Word16 )

-- | Tree storing the code used for huffman encoding.
data HuffmanTree = Branch HuffmanTree HuffmanTree -- ^ If bit is 0 take the first subtree, if 1, the right.
                 | Leaf Word8       -- ^ We should output the value
                 | Empty            -- ^ no value present
                 deriving (Eq, Show)

type HuffmanPackedTree = SV.Vector Word16

-- | Enumeration used to search in the tables for different components.
data DctComponent = DcComponent | AcComponent
    deriving (Eq, Show)

-- WARN! If the tree is malformed, the code will blow up
-- with runtime exception:
-- evalState (build 0) $ [(0,1), (1,3), (2,3)]
build :: Int -> State [(Word8,Int)] HuffmanTree
build n = do
        isEmpty <- liftM null get
        if isEmpty then return Empty -- odd number of leaves
        else do
            (v,s):xs <- get
            if n==s -- leaf
            then put xs >> return (Leaf v)
            else do x <- build (n+1)
                    y <- build (n+1)
                    return $ Branch x y

prepareHuffTree :: [[Word8]] -> HuffmanPackedTree
prepareHuffTree = packHuffmanTree . buildHuffmanTree

buildHuffmanTree :: [[Word8]] -> HuffmanTree
buildHuffmanTree =  evalState (build 0) . concat . zipWith f [1..16]
         where  f s = map (\v->(v,s))

packHuffmanTree :: HuffmanTree -> HuffmanPackedTree
packHuffmanTree tree = runST $ do
    table <- M.replicate 512 0x8000
    let aux (Empty) idx = return $ idx + 1
        aux (Leaf v) idx = do
            table `M.unsafeWrite` idx $ fromIntegral v .|. 0x4000
            return $ idx + 1

        aux (Branch i1@(Leaf _) i2@(Leaf _)) idx =
            aux i1 idx >>= aux i2

        aux (Branch i1@(Leaf _) i2) idx = do
            _ <- aux i1 idx
            ix2 <- aux i2 $ idx + 2
            (table `M.unsafeWrite` (idx + 1)) $ fromIntegral $ idx + 2
            return ix2

        aux (Branch i1 i2@(Leaf _)) idx = do
            ix1 <- aux i1 (idx + 2)
            _ <- aux i2 (idx + 1)
            (table `M.unsafeWrite` idx) . fromIntegral $ idx + 2
            return ix1

        aux (Branch i1 i2) idx = do
            ix1 <- aux i1 (idx + 2)
            ix2 <- aux i2 ix1
            (table `M.unsafeWrite` idx) (fromIntegral $ idx + 2)
            (table `M.unsafeWrite` (idx + 1)) (fromIntegral ix1)
            return ix2
    _ <- aux tree 0
    SV.unsafeFreeze table
