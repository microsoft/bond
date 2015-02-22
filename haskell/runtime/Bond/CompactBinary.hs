{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, MultiWayIf, MultiParamTypeClasses, EmptyDataDecls #-}
module Bond.CompactBinary (
    runCompactBinaryV1Get,
    runCompactBinaryV1Put,
    runCompactBinaryGet,
    runCompactBinaryPut,
    CompactBinaryV1Proto,
    CompactBinaryProto,
    EncodedInt(..), -- export for testing
    encodeInt,      -- export for testing
    decodeInt       -- export for testing
  ) where

import Bond.BinaryProto
import Bond.Types
import Bond.Wire
import Control.Applicative
import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString.Lazy as Lazy

data CompactBinaryV1Proto
data CompactBinaryProto

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

putEncodedWord :: EncodedWord -> BondPut t
putEncodedWord (EncodedWord i) | i < 128 = BondPut $ putWord8 $ fromIntegral i
putEncodedWord (EncodedWord i) = do
    let iLow = fromIntegral $ i .&. 0x7F
    BondPut $ putWord8 $ iLow `setBit` 7
    putEncodedWord $ EncodedWord (i `shiftR` 7)

getEncodedWord :: BondGet t EncodedWord
getEncodedWord = EncodedWord <$> step 0
    where
    step :: Int -> BondGet t Word64
    step n | n > 9 = fail "EncodedWord: sequence too long"
    step n = do
        b <- fromIntegral <$> BondGet getWord8
        rest <- if b `testBit` 7 then step (n + 1)  else return (0 :: Word64)
        return $ (b `clearBit` 7) .|. (rest `shiftL` 7)

instance BondBinary CompactBinaryV1Proto EncodedWord where
    bondGet = getEncodedWord
    bondPut = putEncodedWord

instance BondBinary CompactBinaryProto EncodedWord where
    bondGet = getEncodedWord
    bondPut = putEncodedWord

instance BondBinary CompactBinaryV1Proto EncodedInt where
    bondGet = decodeInt <$> bondGet
    bondPut = bondPut . encodeInt

instance BondBinary CompactBinaryProto EncodedInt where
    bondGet = decodeInt <$> bondGet
    bondPut = bondPut . encodeInt

instance BondBinary CompactBinaryV1Proto Unpacked16 where
    bondGet = Unpacked16 <$> BondGet getWord16le
    bondPut = BondPut . putWord16le . unpack16

instance BondBinary CompactBinaryProto Unpacked16 where
    bondGet = Unpacked16 <$> BondGet getWord16le
    bondPut = BondPut . putWord16le . unpack16

instance BondBinary CompactBinaryV1Proto Word16 where
    bondGet = fromIntegral . unWord <$> bondGet
    bondPut = bondPut . EncodedWord . fromIntegral

instance BondBinary CompactBinaryProto Word16 where
    bondGet = fromIntegral . unWord <$> bondGet
    bondPut = bondPut . EncodedWord . fromIntegral

instance BondBinary CompactBinaryV1Proto Word32 where
    bondGet = fromIntegral . unWord <$> bondGet
    bondPut = bondPut . EncodedWord . fromIntegral

instance BondBinary CompactBinaryProto Word32 where
    bondGet = fromIntegral . unWord <$> bondGet
    bondPut = bondPut . EncodedWord . fromIntegral

instance BondBinary CompactBinaryV1Proto Word64 where
    bondGet = fromIntegral . unWord <$> bondGet
    bondPut = bondPut . EncodedWord . fromIntegral

instance BondBinary CompactBinaryProto Word64 where
    bondGet = fromIntegral . unWord <$> bondGet
    bondPut = bondPut . EncodedWord . fromIntegral

instance BondBinary CompactBinaryV1Proto Int16 where
    bondGet = fromIntegral . unInt . decodeInt <$> bondGet
    bondPut = bondPut . encodeInt . EncodedInt . fromIntegral

instance BondBinary CompactBinaryProto Int16 where
    bondGet = fromIntegral . unInt . decodeInt <$> bondGet
    bondPut = bondPut . encodeInt . EncodedInt . fromIntegral

instance BondBinary CompactBinaryV1Proto Int32 where
    bondGet = fromIntegral . unInt . decodeInt <$> bondGet
    bondPut = bondPut . encodeInt . EncodedInt . fromIntegral

instance BondBinary CompactBinaryProto Int32 where
    bondGet = fromIntegral . unInt . decodeInt <$> bondGet
    bondPut = bondPut . encodeInt . EncodedInt . fromIntegral

instance BondBinary CompactBinaryV1Proto Int64 where
    bondGet = fromIntegral . unInt . decodeInt <$> bondGet
    bondPut = bondPut . encodeInt . EncodedInt . fromIntegral

instance BondBinary CompactBinaryProto Int64 where
    bondGet = fromIntegral . unInt . decodeInt <$> bondGet
    bondPut = bondPut . encodeInt . EncodedInt . fromIntegral

putFieldTag :: BondBinary t Unpacked16 => FieldTag -> BondPut t
putFieldTag (FieldTag t (Ordinal o)) | o <= 5 = let tag = fromWireType t
                                                    ordn = fromIntegral o
                                                    v = (ordn `shiftL` 5) .|. tag
                                                in bondPut v
putFieldTag (FieldTag t (Ordinal o)) | o <= 0xff = let tag = fromWireType t .|. 0xc0
                                                       ordn = fromIntegral o :: Word8
                                                    in do
                                                        bondPut tag
                                                        bondPut ordn
putFieldTag (FieldTag t (Ordinal o)) = let tag = fromWireType t .|. 0xe0
                                        in do
                                            bondPut tag
                                            bondPut $ Unpacked16 o

getFieldTag :: BondBinary t Unpacked16 => BondGet t FieldTag
getFieldTag = do
    tag <- bondGet
    let t = toWireType (tag .&. 0x1f)
    let hibits = tag `shiftR` 5
    o <- if t == BT_STOP || t == BT_STOP_BASE
            then return 0
            else if | hibits <= 5 -> return $ fromIntegral hibits
                    | hibits == 6 -> fromIntegral <$> (bondGet :: BondGet t Word8)
                    | otherwise -> unpack16 <$> bondGet
    return $ FieldTag t (Ordinal o)

instance BondBinary CompactBinaryV1Proto FieldTag where
    bondPut = putFieldTag
    bondGet = getFieldTag

instance BondBinary CompactBinaryProto FieldTag where
    bondPut = putFieldTag
    bondGet = getFieldTag

instance BondBinary CompactBinaryV1Proto StringHead where
    bondPut (StringHead n) = bondPut $ VarInt n
    bondGet = do
        VarInt n <- bondGet
        return $ StringHead n

instance BondBinary CompactBinaryProto StringHead where
    bondPut (StringHead n) = bondPut $ VarInt n
    bondGet = do
        VarInt n <- bondGet
        return $ StringHead n

putMapHead :: MapHead -> BondPut t
putMapHead (MapHead (Just tkey) (Just tvalue) n) = do
    bondPut tkey
    bondPut tvalue
    bondPut $ VarInt n
putMapHead _ = fail "internal error: putting map without type info"

getMapHead :: BondGet t MapHead
getMapHead = do
    tkey <- bondGet
    tvalue <- bondGet
    VarInt n <- bondGet
    return $ MapHead (Just tkey) (Just tvalue) n

instance BondBinary CompactBinaryV1Proto MapHead where
    bondPut = putMapHead
    bondGet = getMapHead

instance BondBinary CompactBinaryProto MapHead where
    bondPut = putMapHead
    bondGet = getMapHead

instance BondBinary CompactBinaryV1Proto ListHead where
    bondPut (ListHead (Just t) n) = do
        bondPut t
        bondPut $ VarInt n
    bondPut (ListHead Nothing _) = fail "internal error: putting list without type info"
    bondGet = do
        t <- bondGet
        VarInt n <- bondGet
        return $ ListHead (Just t) n

instance BondBinary CompactBinaryProto ListHead where
    bondPut (ListHead (Just t) n) | n < 7 = let tag = fromWireType t
                                                v = tag .|. fromIntegral ((n + 1) `shiftL` 5)
                                             in bondPut v
    bondPut (ListHead (Just t) n) = do
        bondPut t
        bondPut $ VarInt n
    bondPut (ListHead Nothing _) = fail "internal error: putting list without type info"
    bondGet = do
        tag <- bondGet
        let t = toWireType (tag .&. 0x1f)
        let hibits = tag `shiftR` 5
        n <- if hibits == 0
                then fromVarInt <$> bondGet
                else return $ fromIntegral (hibits - 1)
        return $ ListHead (Just t) n

instance BondBinaryProto CompactBinaryV1Proto where
    skipValue = skipCompactValue
    getBonded = getContainer 1
    putBonded = putContainer 1

instance BondBinaryProto CompactBinaryProto where
    readStruct reader = do
        VarInt _ <- bondGet -- FIXME use "isolate" from Data.Binary >= 0.7.2.0
        reader
    putStruct (BondPut writer) = do
        let bs = runPut writer
        bondPut $ VarInt $ fromIntegral $ Lazy.length bs
        BondPut $ putLazyByteString bs
    skipValue = skipCompactV2Value
    getBonded = getContainer 2
    putBonded = putContainer 2

putContainer :: BondBinaryStruct t a => Word16 -> Bonded a -> BondPut t
putContainer _ (BondedObject v) = bondPut v
putContainer pver (BondedStream s proto ver)
    | proto == compactSig && ver == pver = BondPut $ putLazyByteString s
    | otherwise = fail "internal error: invalid stream format in BondedStream"

getContainer :: forall a t. (BondBinaryProto t, BondBinaryStruct t a) => Word16 -> BondGet t (Bonded a)
getContainer ver = do
    let BondGet structSize = do
                startpos <- BondGet bytesRead
                skipValue BT_STRUCT :: BondGet t ()
                endpos <- BondGet bytesRead
                return (endpos - startpos)
    size <- BondGet $ lookAhead structSize
    bs <- BondGet $ getLazyByteString size
    return $ BondedStream bs compactSig ver

skipVarInt :: BondGet t ()
skipVarInt = loop
    where
    loop = do
        v :: Word8 <- bondGet
        when (v `testBit` 7) loop

skipCompactValue :: (BondBinaryProto t, BondBinary t ListHead) => BondBinary t FieldTag => ItemType -> BondGet t ()
skipCompactValue BT_UINT16 = skipVarInt
skipCompactValue BT_UINT32 = skipVarInt
skipCompactValue BT_UINT64 = skipVarInt
skipCompactValue BT_INT16 = skipVarInt
skipCompactValue BT_INT32 = skipVarInt
skipCompactValue BT_INT64 = skipVarInt
skipCompactValue t = skipBinaryValue t

skipCompactV2Value :: BondBinary CompactBinaryProto FieldTag => ItemType -> BondGet CompactBinaryProto ()
skipCompactV2Value BT_STRUCT = do
    VarInt len <- bondGet
    BondGet $ skip len
skipCompactV2Value t = skipCompactValue t

runCompactBinaryV1Get :: BondGet CompactBinaryV1Proto a -> Lazy.ByteString -> Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, a) 
runCompactBinaryV1Get (BondGet g) = runGetOrFail g

runCompactBinaryV1Put :: BondPut CompactBinaryV1Proto -> Lazy.ByteString
runCompactBinaryV1Put (BondPut p) = runPut p

runCompactBinaryGet :: BondGet CompactBinaryProto a -> Lazy.ByteString -> Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, a) 
runCompactBinaryGet (BondGet g) = runGetOrFail g

runCompactBinaryPut :: BondPut CompactBinaryProto -> Lazy.ByteString
runCompactBinaryPut (BondPut p) = runPut p
