{-# LANGUAGE ScopedTypeVariables #-}
module Codec.Picture.Jpg.Types( MutableMacroBlock
                              , MacroBlock
                              , QuantificationTable
                              , createEmptyMutableMacroBlock
                              , printMacroBlock
                              , printPureMacroBlock
                              , DcCoefficient
                              , dctBlockSize
                              ) where

import Control.Monad.ST( ST )
import Foreign.Storable ( Storable )
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as M

import Data.Int( Int16 )

{-import Debug.Trace-}
import Text.Printf

-- | Represent a compact array of 8 * 8 values. The size
-- is not guarenteed by type system, but if makeMacroBlock is
-- used, everything should be fine size-wise
type MacroBlock a = VS.Vector a

type QuantificationTable = MacroBlock Int16

-- | Type only used to make clear what kind of integer we are carrying
-- Might be transformed into newtype in the future
type DcCoefficient = Int16

-- | Macroblock that can be transformed.
type MutableMacroBlock s a = M.STVector s a

{-# INLINE createEmptyMutableMacroBlock #-}
-- | Create a new macroblock with the good array size
createEmptyMutableMacroBlock :: (Storable a, Num a) => ST s (MutableMacroBlock s a)
createEmptyMutableMacroBlock = M.replicate 64 0

printMacroBlock :: (Storable a, PrintfArg a)
                => MutableMacroBlock s a -> ST s String
printMacroBlock block = pLn 0
    where pLn 64 = return "===============================\n"
          pLn i = do
              v <- block `M.unsafeRead` i
              vn <- pLn (i+1)
              return $ printf (if i `mod` 8 == 0 then "\n%5d " else "%5d ") v ++ vn

printPureMacroBlock :: (Storable a, PrintfArg a) => MacroBlock a -> String
printPureMacroBlock block = pLn 0
    where pLn 64 = "===============================\n"
          pLn i = str ++ pLn (i + 1)
            where str | i `mod` 8 == 0 = printf "\n%5d " v
                      | otherwise = printf "%5d" v
                  v = block VS.! i

{-# INLINE dctBlockSize #-}
dctBlockSize :: Num a => a
dctBlockSize = 8
