{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, MultiWayIf, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleInstances, EmptyDataDecls #-}
module Bond.FastBinary (
    BondBinary(..),
    BondBinaryStruct(..),
    BondBinaryProto(..),
    BondPut,
    BondGet,
    putStructStop,
    putStructStopBase,
    runFastBinaryGet,
    runFastBinaryPut
  ) where

import Bond.Types
import Bond.Wire
import Control.Applicative
import Control.Monad
import Control.Monad.ST (runST, ST)
import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.Hashable
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.HashSet as H
import qualified Data.Map as M
import qualified Data.Vector as V

newtype VarInt = VarInt Int

data FastBinaryProto
newtype BondGet t a = BondGet (Get a)
    deriving (Functor, Applicative, Monad)
newtype BondPutM t a = BondPut (PutM a)
    deriving (Functor, Applicative, Monad)
type BondPut t = BondPutM t ()

class BondBinaryStruct t a where
    bondGetBase :: BondGet t a
    bondPutBase :: a -> BondPut t

class BondBinary t a where
    bondGet :: BondGet t a
    bondPut :: a -> BondPut t

instance BondBinary t Bool where
    bondGet = do
        v <- BondGet getWord8
        return (v /= 0)
    bondPut v = if v then BondPut (putWord8 1) else BondPut (putWord8 0)

instance BondBinary t Word8 where
    bondGet = BondGet getWord8
    bondPut = BondPut . putWord8

instance BondBinary FastBinaryProto Word16 where
    bondGet = BondGet getWord16le
    bondPut = BondPut . putWord16le

instance BondBinary FastBinaryProto Word32 where
    bondGet = BondGet getWord32le
    bondPut = BondPut . putWord32le

instance BondBinary FastBinaryProto Word64 where
    bondGet = BondGet getWord64le
    bondPut = BondPut . putWord64le

instance BondBinary t Int8 where
    bondGet = BondGet (fromIntegral <$> getWord8)
    bondPut = BondPut . putWord8 . fromIntegral

instance BondBinary FastBinaryProto Int16 where
    bondGet = BondGet (fromIntegral <$> getWord16le)
    bondPut = BondPut . putWord16le . fromIntegral

instance BondBinary FastBinaryProto Int32 where
    bondGet = BondGet (fromIntegral <$> getWord32le)
    bondPut = BondPut . putWord32le . fromIntegral

instance BondBinary FastBinaryProto Int64 where
    bondGet = BondGet (fromIntegral <$> getWord64le)
    bondPut = BondPut . putWord64le . fromIntegral

instance BondBinary t VarInt where
    bondGet = VarInt <$> step 0
        where
        step :: Int -> BondGet t Int
        step n | n > 4 = fail "VarInt: sequence too long"
        step n = do
            b <- fromIntegral <$> BondGet getWord8
            rest <- if b `testBit` 7 then step (n + 1)  else return (0 :: Int)
            return $ (b `clearBit` 7) .|. (rest `shiftL` 7)
    bondPut (VarInt i) | i < 0 = fail "VarInt with negative value"
    bondPut (VarInt i) | i < 128 = BondPut $ putWord8 $ fromIntegral i
    bondPut (VarInt i) = do
        let iLow = fromIntegral $ i .&. 0x7F
        BondPut $ putWord8 $ iLow `setBit` 7
        bondPut $ VarInt (i `shiftR` 7)

instance BondBinary t Double where
    bondGet = BondGet (wordToDouble <$> getWord64le)
    bondPut = BondPut . putWord64le . doubleToWord

instance BondBinary t Float where
    bondGet = BondGet (wordToFloat <$> getWord32le)
    bondPut = BondPut . putWord32le . floatToWord

instance BondBinary t ItemType where
    bondGet = toWireType <$> bondGet
    bondPut = bondPut . fromWireType

instance BondBinary FastBinaryProto FieldTag where
    bondPut (FieldTag t (Ordinal o)) = do
        bondPut t
        bondPut o
    bondGet = do
        t <- bondGet
        o <- if t == BT_STOP || t == BT_STOP_BASE
                then return 0
                else bondGet
        return $ FieldTag t (Ordinal o)

instance (BondBinary t a, WireType a) => BondBinary t (Maybe a) where
    bondPut Nothing = do
        bondPut $ getWireType (undefined :: a)
        bondPut $ VarInt 0
    bondPut (Just v) = do
        bondPut $ getWireType v
        bondPut $ VarInt 1
        bondPut v
    bondGet = do
        t <- bondGet
        when (t /= getWireType (undefined :: a)) $ fail "nullable: type mismatch"
        VarInt n <- bondGet
        if | n == 0 -> return Nothing
           | n == 1 -> Just <$> bondGet
           | otherwise -> fail "bondGet nullable: count isn't 0 or 1"

instance (BondBinary t a, WireType a) => BondBinary t [a] where
    bondPut xs = do
        bondPut $ getWireType $ head xs
        bondPut $ VarInt $ length xs
        mapM_ bondPut xs
    bondGet = do
        t <- bondGet
        when (t /= getWireType (undefined :: a)) $ fail "bondGet [a]: type mismatch"
        VarInt n <- bondGet
        replicateM n bondGet

instance BondBinary t Blob where
    bondPut (Blob s) = do
        bondPut BT_INT8
        bondPut $ VarInt $ BS.length s
        BondPut $ putByteString s
    bondGet = do
        t <- bondGet
        when (t /= BT_INT8) $ fail "bondGet Blob: type mismatch"
        VarInt n <- bondGet
        BondGet (Blob <$> getByteString n)

instance BondBinary t Utf8 where
    bondPut (Utf8 s) = do
        bondPut $ VarInt $ BS.length s
        BondPut $ putByteString s
    bondGet = do
        VarInt n <- bondGet
        Utf8 <$> (BondGet $ getByteString n)

instance BondBinary t Utf16 where
    bondPut (Utf16 s) = do
        bondPut $ VarInt $ BS.length s `div` 2
        BondPut $ putByteString s
    bondGet = do
        VarInt n <- bondGet
        Utf16 <$> (BondGet $ getByteString (n * 2))

instance (Hashable a, Eq a, BondBinary t a, WireType a) => BondBinary t (HashSet a) where
    bondPut xs = do
        bondPut $ getWireType (undefined :: a)
        bondPut $ VarInt $ H.size xs
        mapM_ bondPut $ H.toList xs
    bondGet = do
        t <- bondGet
        when (t /= getWireType (undefined :: a)) $ fail "bondGet (HashSet a): type mismatch"
        VarInt n <- bondGet
        H.fromList <$> replicateM n bondGet

instance (Ord a, BondBinary t a, WireType a, BondBinary t b, WireType b) => BondBinary t (Map a b) where
    bondPut xs = do
        bondPut $ getWireType (undefined :: a)
        bondPut $ getWireType (undefined :: b)
        bondPut $ VarInt $ M.size xs
        forM_ (M.toList xs) $ \(k, v) -> do
            bondPut k
            bondPut v
    bondGet = do
        tkey <- bondGet
        tval <- bondGet
        when (tkey /= getWireType (undefined :: a)) $ fail "bondGet (Map a b): key type mismatch"
        when (tval /= getWireType (undefined :: b)) $ fail "bondGet (Map a b): value type mismatch"
        VarInt n <- bondGet
        fmap M.fromList $ replicateM n $ do
            k <- bondGet
            v <- bondGet
            return (k, v)

instance (BondBinary t a, WireType a) => BondBinary t (Vector a) where
    bondPut xs = do
        bondPut $ getWireType (undefined :: a)
        bondPut $ VarInt $ V.length xs
        V.mapM_ bondPut xs
    bondGet = do
        t <- bondGet
        when (t /= getWireType (undefined :: a)) $ fail "bondGet (Vector a): type mismatch"
        VarInt n <- bondGet
        V.replicateM n bondGet

instance BondBinary t a => BondBinary t (Bonded a) where
    bondPut (Bonded v) = bondPut v
    bondGet = Bonded <$> bondGet

putStructStop :: BondBinaryProto t => BondPut t
putStructStop = bondPut BT_STOP

putStructStopBase :: BondBinaryProto t => BondPut t
putStructStopBase = bondPut BT_STOP_BASE

class (BondBinary t Int8, BondBinary t Int16, BondBinary t Int32, BondBinary t Int64,
       BondBinary t Word8, BondBinary t Word16, BondBinary t Word32, BondBinary t Word64) =>
        BondBinaryProto t where
    checkTypeAndGet :: (BondBinary t a, WireType a) => ItemType -> BondGet t a
    putField :: (BondBinary t a, WireType a) => Ordinal -> a -> BondPut t
    putMaybeField :: (BondBinary t a, WireType a) => Ordinal -> Maybe a -> BondPut t
    readFieldsWith :: (a -> ItemType -> Ordinal -> BondGet t a) -> a -> BondGet t a
    readBaseFieldsWith :: (a -> ItemType -> Ordinal -> BondGet t a) -> a -> BondGet t a
    putEnumValue :: Int32 -> BondPut t
    getEnumValue :: BondGet t Int32
    skipValue :: ItemType -> BondGet t ()

instance BondBinaryProto FastBinaryProto where
    checkTypeAndGet = checkTypeAndGet'
    putField = putField'
    putMaybeField = putMaybeField'
    readFieldsWith = readFieldsWith' BT_STOP
    readBaseFieldsWith = readFieldsWith' BT_STOP_BASE
    putEnumValue = bondPut
    getEnumValue = bondGet
    skipValue = skipValue'

checkTypeAndGet' :: forall t a . (BondBinary t a, WireType a) => ItemType -> BondGet t a
checkTypeAndGet' t | t == getWireType (undefined :: a) = bondGet
checkTypeAndGet' t = fail $ "invalid field type " ++ show t ++ " expected " ++ show (getWireType (undefined :: a))

putField' :: (BondBinary t FieldTag, BondBinary t a, WireType a) => Ordinal -> a -> BondPut t
putField' n f = do
    bondPut $ FieldTag (getWireType f) n
    bondPut f

putMaybeField' :: (BondBinary t FieldTag, BondBinary t a, WireType a) => Ordinal -> Maybe a -> BondPut t
putMaybeField' _ Nothing = return ()
putMaybeField' n (Just f) = putField' n f

readFieldsWith' :: (BondBinary t FieldTag) => ItemType -> (a -> ItemType -> Ordinal -> BondGet t a) -> a -> BondGet t a
readFieldsWith' stop updateFunc = loop
    where
    loop v = do
        FieldTag t n <- bondGet
        if t == stop
            then return v
            else do
                    v' <- updateFunc v t n
                    loop v'

toWireType :: Word8 -> ItemType
toWireType = toEnum . fromIntegral

fromWireType :: ItemType -> Word8
fromWireType = fromIntegral . fromEnum

skipValue' :: BondBinary t FieldTag => ItemType -> BondGet t ()
skipValue' BT_STOP = fail "internal error: skipValue BT_STOP"
skipValue' BT_STOP_BASE = fail "internal error: skipValue BT_STOP_BASE"
skipValue' BT_BOOL = BondGet $ skip 1
skipValue' BT_UINT8 = BondGet $ skip 1
skipValue' BT_UINT16 = BondGet $ skip 2
skipValue' BT_UINT32 = BondGet $ skip 4
skipValue' BT_UINT64 = BondGet $ skip 8
skipValue' BT_FLOAT = BondGet $ skip 4
skipValue' BT_DOUBLE = BondGet $ skip 8
skipValue' BT_INT8 = BondGet $ skip 1
skipValue' BT_INT16 = BondGet $ skip 2
skipValue' BT_INT32 = BondGet $ skip 4
skipValue' BT_INT64 = BondGet $ skip 8
skipValue' BT_STRING = do
    VarInt n <- bondGet
    BondGet $ skip n
skipValue' BT_WSTRING = do
    VarInt n <- bondGet
    BondGet $ skip (n * 2)
skipValue' BT_LIST = do
    t <- bondGet
    VarInt n <- bondGet
    replicateM_ n (skipValue' t)
skipValue' BT_SET = skipValue' BT_LIST
skipValue' BT_MAP = do
    tkey <- bondGet
    tvalue <- bondGet
    VarInt n <- bondGet
    replicateM_ n $ do
        skipValue' tkey
        skipValue' tvalue
skipValue' BT_STRUCT = loop
    where
    loop = do
        FieldTag t _ <- bondGet
        case t of
            BT_STOP -> return ()
            BT_STOP_BASE -> loop
            _ -> do
                    skipValue' t
                    loop

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

runFastBinaryGet :: BondGet FastBinaryProto a -> Lazy.ByteString -> Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, a) 
runFastBinaryGet (BondGet g) = runGetOrFail g

runFastBinaryPut :: BondPut FastBinaryProto -> Lazy.ByteString
runFastBinaryPut (BondPut p) = runPut p
