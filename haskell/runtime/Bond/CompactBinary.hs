{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, MultiWayIf, MultiParamTypeClasses, FlexibleInstances, EmptyDataDecls #-}
module Bond.CompactBinary (
    runCompactBinaryV1Get,
    runCompactBinaryV1Put,
    runCompactBinaryV2Get,
    runCompactBinaryV2Put,
    EncodedInt(..), -- export for testing
    encodeInt,      -- export for testing
    decodeInt       -- export for testing
  ) where

import Bond.BinaryProto
import Bond.Types
import Bond.Wire
import Control.Applicative
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString.Lazy as Lazy

data CompactBinaryV1Proto
data CompactBinaryV2Proto

class CompactBinaryProto t
instance CompactBinaryProto CompactBinaryV1Proto
instance CompactBinaryProto CompactBinaryV2Proto

newtype EncodedWord = EncodedWord { unWord :: Word64 }
newtype EncodedInt = EncodedInt { unInt :: Int64 }
    deriving (Show, Eq)
newtype Unpacked16 = Unpacked16 { unpack16 :: Word16 }

encodeInt :: EncodedInt -> EncodedWord
encodeInt (EncodedInt i) | i >= 0 = EncodedWord $ 2 * fromIntegral i
encodeInt (EncodedInt i) = EncodedWord $ (2 * fromIntegral (abs i)) - 1

decodeInt :: EncodedWord -> EncodedInt
decodeInt (EncodedWord w) | even w = EncodedInt $ fromIntegral (w `div` 2)
decodeInt (EncodedWord w) = EncodedInt $ negate $ fromIntegral ((w - 1) `div` 2) + 1

instance CompactBinaryProto t => BondBinary t EncodedWord where
    bondGet = EncodedWord <$> step 0
        where
        step :: Int -> BondGet t Word64
        step n | n > 9 = fail "EncodedWord: sequence too long"
        step n = do
            b <- fromIntegral <$> BondGet getWord8
            rest <- if b `testBit` 7 then step (n + 1)  else return (0 :: Word64)
            return $ (b `clearBit` 7) .|. (rest `shiftL` 7)
    bondPut (EncodedWord i) | i < 128 = BondPut $ putWord8 $ fromIntegral i
    bondPut (EncodedWord i) = do
        let iLow = fromIntegral $ i .&. 0x7F
        BondPut $ putWord8 $ iLow `setBit` 7
        bondPut $ EncodedWord (i `shiftR` 7)

instance CompactBinaryProto t => BondBinary t EncodedInt where
    bondGet = decodeInt <$> bondGet
    bondPut = bondPut . encodeInt

instance CompactBinaryProto t => BondBinary t Unpacked16 where
    bondGet = Unpacked16 <$> BondGet getWord16le
    bondPut = BondPut . putWord16le . unpack16

instance CompactBinaryProto t => BondBinary t Word16 where
    bondGet = fromIntegral . unWord <$> bondGet
    bondPut = bondPut . EncodedWord . fromIntegral

instance CompactBinaryProto t => BondBinary t Word32 where
    bondGet = fromIntegral . unWord <$> bondGet
    bondPut = bondPut . EncodedWord . fromIntegral

instance CompactBinaryProto t => BondBinary t Word64 where
    bondGet = fromIntegral . unWord <$> bondGet
    bondPut = bondPut . EncodedWord . fromIntegral

instance CompactBinaryProto t => BondBinary t Int16 where
    bondGet = fromIntegral . unInt . decodeInt <$> bondGet
    bondPut = bondPut . encodeInt . EncodedInt . fromIntegral

instance CompactBinaryProto t => BondBinary t Int32 where
    bondGet = fromIntegral . unInt . decodeInt <$> bondGet
    bondPut = bondPut . encodeInt . EncodedInt . fromIntegral

instance CompactBinaryProto t => BondBinary t Int64 where
    bondGet = fromIntegral . unInt . decodeInt <$> bondGet
    bondPut = bondPut . encodeInt . EncodedInt . fromIntegral

instance CompactBinaryProto t => BondBinary t FieldTag where
    bondPut (FieldTag t (Ordinal o)) | o <= 5 = let tag = fromWireType t
                                                    ordn = fromIntegral o
                                                    v = (ordn `shiftL` 5) .|. tag
                                                in bondPut v
    bondPut (FieldTag t (Ordinal o)) | o <= 0xff = let tag = fromWireType t .|. 0xc0
                                                       ordn = fromIntegral o :: Word8
                                                    in do
                                                        bondPut tag
                                                        bondPut ordn
    bondPut (FieldTag t (Ordinal o)) = let tag = fromWireType t .|. 0xe0
                                        in do
                                            bondPut tag
                                            bondPut $ Unpacked16 o
    bondGet = do
        tag <- bondGet
        let t = toWireType (tag .&. 0x1f)
        let hibits = tag `shiftR` 5
        o <- if t == BT_STOP || t == BT_STOP_BASE
                then return 0
                else if | hibits <= 5 -> return $ fromIntegral hibits
                        | hibits == 6 -> fromIntegral <$> (bondGet :: BondGet t Word8)
                        | otherwise -> unpack16 <$> bondGet
        return $ FieldTag t (Ordinal o)

instance BondBinary CompactBinaryV1Proto ListHead where
    bondPut (ListHead t n) = do
        bondPut t
        bondPut $ VarInt n
    bondGet = do
        t <- bondGet
        VarInt n <- bondGet
        return $ ListHead t n

instance BondBinary CompactBinaryV2Proto ListHead where
    bondPut (ListHead t n) | n < 7 = let tag = fromWireType t
                                         v = tag .|. fromIntegral ((n + 1) `shiftL` 5)
                                      in bondPut v
    bondPut (ListHead t n) = do
        bondPut t
        bondPut $ VarInt n
    bondGet = do
        tag <- bondGet
        let t = toWireType (tag .&. 0x1f)
        let hibits = tag `shiftR` 5
        n <- if hibits == 0
                then fromVarInt <$> bondGet
                else return $ fromIntegral (hibits - 1)
        return $ ListHead t n

instance BondBinaryProto CompactBinaryV1Proto where
    skipValue = skipCompactValue

instance BondBinaryProto CompactBinaryV2Proto where
    readStruct reader = do
        VarInt _ <- bondGet -- FIXME use "isolate" from Data.Binary >= 0.7.2.0
        reader
    putStruct (BondPut writer) = do
        let bs = runPut writer
        bondPut $ VarInt $ fromIntegral $ Lazy.length bs
        BondPut $ putLazyByteString bs
    skipValue = skipCompactV2Value

skipVarInt :: BondGet t ()
skipVarInt = loop
    where
    loop = do
        v :: Word8 <- bondGet
        if v `testBit` 7 then loop else return ()

skipCompactValue :: (BondBinaryProto t, CompactBinaryProto t, BondBinary t ListHead) => BondBinary t FieldTag => ItemType -> BondGet t ()
skipCompactValue BT_UINT16 = skipVarInt
skipCompactValue BT_UINT32 = skipVarInt
skipCompactValue BT_UINT64 = skipVarInt
skipCompactValue BT_INT16 = skipVarInt
skipCompactValue BT_INT32 = skipVarInt
skipCompactValue BT_INT64 = skipVarInt
skipCompactValue t = skipBinaryValue t

skipCompactV2Value :: BondBinary CompactBinaryV2Proto FieldTag => ItemType -> BondGet CompactBinaryV2Proto ()
skipCompactV2Value BT_STRUCT = do
    VarInt len <- bondGet
    BondGet $ skip len
skipCompactV2Value t = skipCompactValue t

runCompactBinaryV1Get :: BondGet CompactBinaryV1Proto a -> Lazy.ByteString -> Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, a) 
runCompactBinaryV1Get (BondGet g) = runGetOrFail g

runCompactBinaryV1Put :: BondPut CompactBinaryV1Proto -> Lazy.ByteString
runCompactBinaryV1Put (BondPut p) = runPut p

runCompactBinaryV2Get :: BondGet CompactBinaryV2Proto a -> Lazy.ByteString -> Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, a) 
runCompactBinaryV2Get (BondGet g) = runGetOrFail g

runCompactBinaryV2Put :: BondPut CompactBinaryV2Proto -> Lazy.ByteString
runCompactBinaryV2Put (BondPut p) = runPut p
