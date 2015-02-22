{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, MultiWayIf, MultiParamTypeClasses, FlexibleInstances, EmptyDataDecls #-}
module Bond.SimpleBinary (
    SimpleBinaryProto,
    SimpleBinaryV1Proto,
    runSimpleBinaryGet,
    runSimpleBinaryPut,
    runSimpleBinaryV1Get,
    runSimpleBinaryV1Put
  ) where

import Bond.BinaryProto
import Bond.Schema
import Bond.Types
import Bond.Wire
import Control.Applicative
import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import Data.Proxy
import qualified Data.ByteString.Lazy as Lazy

data SimpleBinaryV1Proto
data SimpleBinaryProto

instance BondBinary SimpleBinaryV1Proto Word16 where
    bondGet = BondGet getWord16le
    bondPut = BondPut . putWord16le

instance BondBinary SimpleBinaryV1Proto Word32 where
    bondGet = BondGet getWord32le
    bondPut = BondPut . putWord32le

instance BondBinary SimpleBinaryV1Proto Word64 where
    bondGet = BondGet getWord64le
    bondPut = BondPut . putWord64le

instance BondBinary SimpleBinaryV1Proto Int16 where
    bondGet = BondGet (fromIntegral <$> getWord16le)
    bondPut = BondPut . putWord16le . fromIntegral

instance BondBinary SimpleBinaryV1Proto Int32 where
    bondGet = BondGet (fromIntegral <$> getWord32le)
    bondPut = BondPut . putWord32le . fromIntegral

instance BondBinary SimpleBinaryV1Proto Int64 where
    bondGet = BondGet (fromIntegral <$> getWord64le)
    bondPut = BondPut . putWord64le . fromIntegral

instance BondBinary SimpleBinaryV1Proto FieldTag where
    bondPut _ = return ()
    bondGet = fail "internal error: can't read field tag from SimpleBinary"

instance BondBinary SimpleBinaryV1Proto ListHead where
    bondPut (ListHead _ n) = bondPut (fromIntegral n :: Word32)
    bondGet = do
        (n :: Word32) <- bondGet
        return $ ListHead Nothing (fromIntegral n)

instance BondBinary SimpleBinaryV1Proto MapHead where
    bondPut (MapHead _ _ n) = bondPut (fromIntegral n :: Word32)
    bondGet = do
        n <- BondGet getWord32le
        return $ MapHead Nothing Nothing (fromIntegral n)

instance BondBinary SimpleBinaryV1Proto StringHead where
    bondPut (StringHead n) = bondPut (fromIntegral n :: Word32)
    bondGet = do
        n <- BondGet getWord32le
        return $ StringHead (fromIntegral n)

instance BondBinaryProto SimpleBinaryV1Proto where
    readFieldsWith = readSimpleBinaryStruct
    readBaseFieldsWith = readSimpleBinaryStruct
    checkTypeAndGet = getSimpleBinaryValue
    getBonded = getBondedContainer
    putBonded = putBondedContainerV1
    putField _ _ = bondPut
    putMaybeField = putMaybeField'
    putStructField _ = bondPut
    putStructStop = return ()
    putStructStopBase = return ()
    protoSignature = const simpleV1Sig

instance BondBinary SimpleBinaryProto Word16 where
    bondGet = BondGet getWord16le
    bondPut = BondPut . putWord16le

instance BondBinary SimpleBinaryProto Word32 where
    bondGet = BondGet getWord32le
    bondPut = BondPut . putWord32le

instance BondBinary SimpleBinaryProto Word64 where
    bondGet = BondGet getWord64le
    bondPut = BondPut . putWord64le

instance BondBinary SimpleBinaryProto Int16 where
    bondGet = BondGet (fromIntegral <$> getWord16le)
    bondPut = BondPut . putWord16le . fromIntegral

instance BondBinary SimpleBinaryProto Int32 where
    bondGet = BondGet (fromIntegral <$> getWord32le)
    bondPut = BondPut . putWord32le . fromIntegral

instance BondBinary SimpleBinaryProto Int64 where
    bondGet = BondGet (fromIntegral <$> getWord64le)
    bondPut = BondPut . putWord64le . fromIntegral

instance BondBinary SimpleBinaryProto FieldTag where
    bondPut _ = return ()
    bondGet = fail "internal error: can't read field tag from SimpleBinary"

instance BondBinary SimpleBinaryProto ListHead where
    bondPut (ListHead _ n) = bondPut $ VarInt n
    bondGet = do
        VarInt n <- bondGet
        return $ ListHead Nothing n

instance BondBinary SimpleBinaryProto MapHead where
    bondPut (MapHead _ _ n) = bondPut $ VarInt n
    bondGet = do
        VarInt n <- bondGet
        return $ MapHead Nothing Nothing n

instance BondBinary SimpleBinaryProto StringHead where
    bondPut (StringHead n) = bondPut $ VarInt n
    bondGet = do
        VarInt n <- bondGet
        return $ StringHead n

instance BondBinaryProto SimpleBinaryProto where
    readFieldsWith = readSimpleBinaryStruct
    readBaseFieldsWith = readSimpleBinaryStruct
    checkTypeAndGet = getSimpleBinaryValue
    getBonded = getBondedContainer
    putBonded = putBondedContainer
    putField _ _ = bondPut
    putMaybeField = putMaybeField'
    putStructField _ = bondPut
    putStructStop = return ()
    putStructStopBase = return ()
    protoSignature = const simpleSig

getBondedContainer :: BondGet t (Bonded a)
getBondedContainer = do
    size <- BondGet getWord32le
    proto <- ProtoSig <$> BondGet getWord32be
    bs <- BondGet $ getLazyByteString (fromIntegral $ size - 4)
    return $ BondedStream proto bs

putContainer :: ProtoSig -> Lazy.ByteString -> BondPut t
putContainer (ProtoSig proto) s = do
    BondPut $ putWord32le $ fromIntegral (4 + Lazy.length s)
    BondPut $ putWord32be proto
    BondPut $ putLazyByteString s

putBondedContainerV1 :: forall a. BondBinaryStruct SimpleBinaryV1Proto a => Bonded a -> BondPut SimpleBinaryV1Proto
putBondedContainerV1 (BondedStream proto s) = putContainer proto s
putBondedContainerV1 (BondedObject a) = putContainer simpleV1Sig (runSimpleBinaryV1Put $ bondPut a)

putBondedContainer :: forall a. BondBinaryStruct SimpleBinaryProto a => Bonded a -> BondPut SimpleBinaryProto
putBondedContainer (BondedStream proto s) = putContainer proto s
putBondedContainer (BondedObject a) = putContainer simpleSig (runSimpleBinaryPut $ bondPut a)

readSimpleBinaryStruct :: forall t a. BondBinaryStruct t a => (a -> ItemType -> Ordinal -> BondGet t a) -> a -> BondGet t a
readSimpleBinaryStruct update r = foldM (\v (FieldInfo _ o) -> update v undefined o) r schema
    where
    StructSchema schema = bondGetSchema (Proxy :: Proxy (t, a))

getSimpleBinaryValue :: (BondBinary t a, WireType a) => ItemType -> BondGet t a
getSimpleBinaryValue _ = bondGet

putMaybeField' :: BondBinary t a => Ordinal -> Maybe a -> BondPut t
putMaybeField' _ Nothing = fail "Can't put defaultNothing to SimpleBinaryV1 protocol"
putMaybeField' _ (Just f) = bondPut f

runSimpleBinaryV1Get :: BondGet SimpleBinaryV1Proto a -> Lazy.ByteString -> Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, a) 
runSimpleBinaryV1Get (BondGet g) = runGetOrFail g

runSimpleBinaryV1Put :: BondPut SimpleBinaryV1Proto -> Lazy.ByteString
runSimpleBinaryV1Put (BondPut p) = runPut p

runSimpleBinaryGet :: BondGet SimpleBinaryProto a -> Lazy.ByteString -> Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, a) 
runSimpleBinaryGet (BondGet g) = runGetOrFail g

runSimpleBinaryPut :: BondPut SimpleBinaryProto -> Lazy.ByteString
runSimpleBinaryPut (BondPut p) = runPut p
