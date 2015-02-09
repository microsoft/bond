{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Bond.FastBinary (
    FastBinary(..),
    FastBinaryM
  ) where

import Bond.Types
import Bond.Wire
import Control.Monad
import Control.Monad.ST (runST, ST)
import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.HashSet as H
import qualified Data.Map as M
import qualified Data.Vector as V

type FastBinaryM = Put

class FastBinary a where
    fastBinaryPut :: a -> FastBinaryM

instance FastBinary Bool where
    fastBinaryPut v = if v then putWord8 1 else putWord8 0
instance FastBinary Double where
    fastBinaryPut = putWord64le . doubleToWord
instance FastBinary Float where
    fastBinaryPut = putWord32le . floatToWord
instance FastBinary Int8 where
    fastBinaryPut = putWord8 . fromIntegral
instance FastBinary Int16 where
    fastBinaryPut = putWord16le . fromIntegral
instance FastBinary Int32 where
    fastBinaryPut = putWord32le . fromIntegral
instance FastBinary Int64 where
    fastBinaryPut = putWord64le . fromIntegral
instance FastBinary Word8 where
    fastBinaryPut = putWord8
instance FastBinary Word16 where
    fastBinaryPut = putWord16le
instance FastBinary Word32 where
    fastBinaryPut = putWord32le
instance FastBinary Word64 where
    fastBinaryPut = putWord64le
instance (FastBinary a, WireType a) => FastBinary (Maybe a) where
    fastBinaryPut Nothing = do
        putWord8 $ wireType (undefined :: a)
        putVarInt 0
    fastBinaryPut (Just v) = do
        putWord8 $ wireType v
        putVarInt 1
        fastBinaryPut v
instance (FastBinary a, WireType a) => FastBinary [a] where
    fastBinaryPut xs = do
        putWord8 $ wireType (undefined :: a)
        putVarInt $ length xs
        mapM_ fastBinaryPut xs
instance FastBinary Blob where
    fastBinaryPut (Blob s) = do
        putWord8 $ wireType (undefined :: Word8)
        putVarInt $ BS.length s
        putByteString s
instance FastBinary Utf8 where
    fastBinaryPut v@(Utf8 s) = do
        putWord8 $ wireType v
        putVarInt $ BS.length s
        putByteString s
instance FastBinary Utf16 where
    fastBinaryPut v@(Utf16 s) = do
        putWord8 $ wireType v
        putVarInt $ BS.length s `div` 2
        putByteString s
instance (FastBinary a, WireType a) => FastBinary (HashSet a) where
    fastBinaryPut xs = do
        putWord8 $ wireType (undefined :: a)
        putVarInt $ H.size xs
        mapM_ fastBinaryPut $ H.toList xs
instance (FastBinary a, WireType a, FastBinary b, WireType b) => FastBinary (Map a b) where
    fastBinaryPut xs = do
        putWord8 $ wireType (undefined :: a)
        putWord8 $ wireType (undefined :: b)
        putVarInt $ M.size xs
        forM_ (M.toList xs) $ \(k, v) -> do
            fastBinaryPut k
            fastBinaryPut v
instance (FastBinary a, WireType a) => FastBinary (Vector a) where
    fastBinaryPut xs = do
        putWord8 $ wireType (undefined :: a)
        putVarInt $ V.length xs
        V.mapM_ fastBinaryPut xs
--instance FastBinary a => Default (Bonded a) where defaultValue = Bonded defaultValue

{-
{-# INLINE wordToFloat #-}
wordToFloat :: Word32 -> Float
wordToFloat x = runST (cast x)
-}
{-# INLINE floatToWord #-}
floatToWord :: Float -> Word32
floatToWord x = runST (cast x)

{-
{-# INLINE wordToDouble #-}
wordToDouble :: Word64 -> Double
wordToDouble x = runST (cast x)
-}

{-# INLINE doubleToWord #-}
doubleToWord :: Double -> Word64
doubleToWord x = runST (cast x)

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) =>
        a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

wireType :: WireType a => a -> Word8
wireType = fromIntegral . fromEnum . getWireType

putVarInt :: Int -> FastBinaryM
putVarInt i | i < 0 = error "putVarInt called with negative value"
putVarInt i | i < 128 = putWord8 $ fromIntegral i
putVarInt i = do
    let iLow = fromIntegral $ i .&. 0x7F
    putWord8 $ iLow .|. 0x80
    putVarInt $ i `shiftR` 7
