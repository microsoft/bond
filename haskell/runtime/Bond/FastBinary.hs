{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, MultiWayIf #-}
module Bond.FastBinary (
    FastBinary(..),
    fastBinaryGetField,
    getFieldsWith,
    getInt32le,
    putField,
    putInt32le,
    putMaybeField,
    putStopBase,
    skipValue
  ) where

import Bond.Types
import Bond.Default
import Bond.Wire
import Control.Monad
import Control.Monad.ST (runST, ST)
import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)
import Data.Binary.Put
import Data.Binary.Get
import Data.Bits
import Data.Functor
import Data.Hashable
import qualified Data.ByteString as BS
import qualified Data.HashSet as H
import qualified Data.Map as M
import qualified Data.Vector as V

type FastBinaryPutM = Put
type FastBinaryGetM = Get

class FastBinary a where
    fastBinaryPut :: a -> FastBinaryPutM
    fastBinaryGet :: FastBinaryGetM a

instance FastBinary Bool where
    fastBinaryPut v = if v then putWord8 1 else putWord8 0
    fastBinaryGet = do
        v <- getWord8
        return (v /= 0)
instance FastBinary Double where
    fastBinaryPut = putWord64le . doubleToWord
    fastBinaryGet = wordToDouble <$> getWord64le
instance FastBinary Float where
    fastBinaryPut = putWord32le . floatToWord
    fastBinaryGet = wordToFloat <$> getWord32le
instance FastBinary Int8 where
    fastBinaryPut = putWord8 . fromIntegral
    fastBinaryGet = fromIntegral <$> getWord8
instance FastBinary Int16 where
    fastBinaryPut = putWord16le . fromIntegral
    fastBinaryGet = fromIntegral <$> getWord16le
instance FastBinary Int32 where
    fastBinaryPut = putWord32le . fromIntegral
    fastBinaryGet = fromIntegral <$> getWord32le
instance FastBinary Int64 where
    fastBinaryPut = putWord64le . fromIntegral
    fastBinaryGet = fromIntegral <$> getWord64le
instance FastBinary Word8 where
    fastBinaryPut = putWord8
    fastBinaryGet = getWord8
instance FastBinary Word16 where
    fastBinaryPut = putWord16le
    fastBinaryGet = getWord16le
instance FastBinary Word32 where
    fastBinaryPut = putWord32le
    fastBinaryGet = getWord32le
instance FastBinary Word64 where
    fastBinaryPut = putWord64le
    fastBinaryGet = getWord64le
instance (FastBinary a, WireType a) => FastBinary (Maybe a) where
    fastBinaryPut Nothing = do
        putWord8 $ wireType (undefined :: a)
        putVarInt 0
    fastBinaryPut (Just v) = do
        putWord8 $ wireType v
        putVarInt 1
        fastBinaryPut v
    fastBinaryGet = do
        t <- getWord8
        when (toWireType t /= getWireType (undefined :: a)) $ error "nullable: type mismatch"
        n <- getVarInt
        if | n == 0 -> return Nothing
           | n == 1 -> Just <$> fastBinaryGet
           | otherwise -> error "fastBinaryGet nullable: count isn't 0 or 1"
instance (FastBinary a, WireType a) => FastBinary [a] where
    fastBinaryPut xs = do
        putWord8 $ wireType (undefined :: a)
        putVarInt $ length xs
        mapM_ fastBinaryPut xs
    fastBinaryGet = do
        t <- getWord8
        when (toWireType t /= getWireType (undefined :: a)) $ error "fastBinaryGet [a]: type mismatch"
        n <- getVarInt
        replicateM n fastBinaryGet
instance FastBinary Blob where
    fastBinaryPut (Blob s) = do
        putWord8 $ fromWireType BT_INT8
        putVarInt $ BS.length s
        putByteString s
    fastBinaryGet = do
        t <- getWord8
        when (toWireType t /= BT_INT8) $ error "fastBinaryGet Blob: type mismatch"
        n <- getVarInt
        Blob <$> getByteString n
instance FastBinary Utf8 where
    fastBinaryPut (Utf8 s) = do
        putVarInt $ BS.length s
        putByteString s
    fastBinaryGet = do
        n <- getVarInt
        Utf8 <$> getByteString n
instance FastBinary Utf16 where
    fastBinaryPut (Utf16 s) = do
        putVarInt $ BS.length s `div` 2
        putByteString s
    fastBinaryGet = do
        n <- getVarInt
        Utf16 <$> getByteString (n * 2)
instance (Hashable a, Eq a, FastBinary a, WireType a) => FastBinary (HashSet a) where
    fastBinaryPut xs = do
        putWord8 $ wireType (undefined :: a)
        putVarInt $ H.size xs
        mapM_ fastBinaryPut $ H.toList xs
    fastBinaryGet = do
        t <- getWord8
        when (toWireType t /= getWireType (undefined :: a)) $ error "fastBinaryGet (HashSet a): type mismatch"
        n <- getVarInt
        H.fromList <$> replicateM n fastBinaryGet
instance (Ord a, FastBinary a, WireType a, FastBinary b, WireType b) => FastBinary (Map a b) where
    fastBinaryPut xs = do
        putWord8 $ wireType (undefined :: a)
        putWord8 $ wireType (undefined :: b)
        putVarInt $ M.size xs
        forM_ (M.toList xs) $ \(k, v) -> do
            fastBinaryPut k
            fastBinaryPut v
    fastBinaryGet = do
        tkey <- getWord8
        tval <- getWord8
        when (toWireType tkey /= getWireType (undefined :: a)) $ error "fastBinaryGet (Map a b): key type mismatch"
        when (toWireType tval /= getWireType (undefined :: b)) $ error "fastBinaryGet (Map a b): value type mismatch"
        n <- getVarInt
        fmap M.fromList $ replicateM n $ do
            k <- fastBinaryGet
            v <- fastBinaryGet
            return (k, v)
instance (FastBinary a, WireType a) => FastBinary (Vector a) where
    fastBinaryPut xs = do
        putWord8 $ wireType (undefined :: a)
        putVarInt $ V.length xs
        V.mapM_ fastBinaryPut xs
    fastBinaryGet = do
        t <- getWord8
        when (toWireType t /= getWireType (undefined :: a)) $ error "fastBinaryGet (Vector a): type mismatch"
        n <- getVarInt
        V.replicateM n fastBinaryGet
instance FastBinary a => FastBinary (Bonded a) where
    fastBinaryPut (Bonded v) = fastBinaryPut v
    fastBinaryGet = Bonded <$> fastBinaryGet

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

wireType :: WireType a => a -> Word8
wireType = fromWireType . getWireType

toWireType :: Word8 -> ItemType
toWireType = toEnum . fromIntegral

fromWireType :: ItemType -> Word8
fromWireType = fromIntegral . fromEnum

putVarInt :: Int -> FastBinaryPutM
putVarInt i | i < 0 = error "putVarInt called with negative value"
putVarInt i | i < 128 = putWord8 $ fromIntegral i
putVarInt i = do
    let iLow = fromIntegral $ i .&. 0x7F
    putWord8 $ iLow `setBit` 7
    putVarInt $ i `shiftR` 7

getVarInt :: FastBinaryGetM Int
getVarInt = step 0
    where
    step :: Int -> FastBinaryGetM Int
    step n | n > 4 = error "getVarInt: sequence too long"
    step n = do
        b <- fromIntegral <$> getWord8
        rest <- if b `testBit` 7 then step (n + 1)  else return (0 :: Int)
        return $ (b `clearBit` 7) .|. (rest `shiftL` 7)

putInt32le :: Int32 -> FastBinaryPutM
putInt32le = putWord32le . fromIntegral

getInt32le :: FastBinaryGetM Int32
getInt32le = fromIntegral <$> getWord32le

putStopBase :: FastBinaryPutM
putStopBase = putWord8 $ fromIntegral $ fromEnum BT_STOP_BASE

putField :: (FastBinary t, WireType t) => Word16 -> t -> FastBinaryPutM
putField n f = do
    let t = getWireType f
    putWord8 $ fromIntegral $ fromEnum t
    putWord16le n
    fastBinaryPut f
    when (t == BT_STRUCT) $ putWord8 $ fromIntegral $ fromEnum BT_STOP

putMaybeField :: (FastBinary t, WireType t) => Word16 -> Maybe t -> FastBinaryPutM
putMaybeField _ Nothing = return ()
putMaybeField n (Just f) = putField n f

getFieldsWith :: Default a => (a -> ItemType -> Word16 -> FastBinaryGetM a) -> a -> FastBinaryGetM a
getFieldsWith updateFunc = loop
    where
    loop v = do
        t <- fmap (toEnum . fromIntegral) getWord8
        process v t
    process v BT_STOP = return v
    process v BT_STOP_BASE = return v
    process v t = do
        n <- getWord16le
        v' <- updateFunc v t n
        loop v'

skipValue :: ItemType -> FastBinaryGetM ()
skipValue BT_STOP = error "skipValue BT_STOP"
skipValue BT_STOP_BASE = error "skipValue BT_STOP_BASE"
skipValue BT_BOOL = skip 1
skipValue BT_UINT8 = skip 1
skipValue BT_UINT16 = skip 2
skipValue BT_UINT32 = skip 4
skipValue BT_UINT64 = skip 8
skipValue BT_FLOAT = skip 4
skipValue BT_DOUBLE = skip 8
skipValue BT_INT8 = skip 1
skipValue BT_INT16 = skip 2
skipValue BT_INT32 = skip 4
skipValue BT_INT64 = skip 8
skipValue BT_STRING = do
    n <- getVarInt
    skip n
skipValue BT_WSTRING = do
    n <- getVarInt
    skip (n * 2)
skipValue BT_LIST = do
    t <- toWireType <$> getWord8
    n <- getVarInt
    replicateM_ n (skipValue t)
skipValue BT_SET = skipValue BT_LIST
skipValue BT_MAP = do
    tkey <- toWireType <$> getWord8
    tvalue <- toWireType <$> getWord8
    n <- getVarInt
    replicateM_ n $ do
        skipValue tkey
        skipValue tvalue
skipValue BT_STRUCT = loop
    where
    loop = do
        t <- fmap (toEnum . fromIntegral) getWord8
        process t
    process BT_STOP = return ()
    process BT_STOP_BASE = loop -- base fields finished, keep going
    process t = do
        skip 2 -- skip ordinal
        skipValue t
        loop

fastBinaryGetField :: forall a . (FastBinary a, WireType a) => ItemType -> FastBinaryGetM a
fastBinaryGetField t | t == getWireType (undefined :: a) = fastBinaryGet
fastBinaryGetField t = error $ "invalid field type " ++ show t ++ " expected " ++ show (getWireType (undefined :: a))
