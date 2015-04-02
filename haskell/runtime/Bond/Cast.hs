{-# Language FlexibleContexts #-}
module Bond.Cast (
        wordToFloat,
        floatToWord,
        wordToDouble,
        doubleToWord
    ) where

import Control.Monad.ST (runST, ST)
import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)
import Data.Word

{-# INLINE wordToFloat #-}
wordToFloat :: Word32 -> Float
wordToFloat x = runST (cast x)

{-# INLINE floatToWord #-}
floatToWord :: Float -> Word32
floatToWord x = runST (cast x)

{-# INLINE wordToDouble #-}
wordToDouble :: Word64 -> Double
wordToDouble x = runST (cast x)

{-# INLINE doubleToWord #-}
doubleToWord :: Double -> Word64
doubleToWord x = runST (cast x)

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) =>
        a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0
