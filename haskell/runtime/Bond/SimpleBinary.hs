{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, MultiWayIf, MultiParamTypeClasses, FlexibleInstances, EmptyDataDecls #-}
module Bond.SimpleBinary (
    runSimpleBinaryGet,
    runSimpleBinaryPut
  ) where

import Bond.BinaryProto
import Bond.Schema
import Bond.Types
import Bond.Wire
import Control.Applicative
import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as Lazy

data SimpleBinaryProto

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
    bondPut (ListHead _ n) = bondPut (fromIntegral n :: Word32)
    bondGet = do
        (n :: Word32) <- bondGet
        return $ ListHead Nothing (fromIntegral n)

instance BondBinary SimpleBinaryProto MapHead where
    bondPut (MapHead _ _ n) = bondPut (fromIntegral n :: Word32)
    bondGet = do
        n <- BondGet getWord32le
        return $ MapHead Nothing Nothing (fromIntegral n)

instance BondBinary SimpleBinaryProto StringHead where
    bondPut (StringHead n) = bondPut (fromIntegral n :: Word32)
    bondGet = do
        n <- BondGet getWord32le
        return $ StringHead (fromIntegral n)

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

getBondedContainer :: BondGet SimpleBinaryProto (Bonded a)
getBondedContainer = do
    size <- BondGet getWord32le
    bs <- BondGet $ getLazyByteString (fromIntegral size)
    return $ BondedStream bs undefined

putBondedContainer :: Bonded a -> BondPut SimpleBinaryProto
putBondedContainer (BondedStream s _) = do
    BondPut $ putWord32le $ fromIntegral (Lazy.length s)
    BondPut $ putLazyByteString s

readSimpleBinaryStruct :: forall a. BondBinaryStruct SimpleBinaryProto a => (a -> ItemType -> Ordinal -> BondGet SimpleBinaryProto a) -> a -> BondGet SimpleBinaryProto a
readSimpleBinaryStruct update r = foldM (\v (FieldInfo _ o) -> update v undefined o) r schema
    where
    StructSchema schema = bondGetSchema r :: StructSchema SimpleBinaryProto

getSimpleBinaryValue :: forall t a . (BondBinary t a, WireType a) => ItemType -> BondGet t a
getSimpleBinaryValue _ = bondGet

putMaybeField' :: BondBinary t a => Ordinal -> Maybe a -> BondPut t
putMaybeField' _ Nothing = fail "Can't put defaultNothing to SimpleBinary protocol"
putMaybeField' _ (Just f) = bondPut f

runSimpleBinaryGet :: BondGet SimpleBinaryProto a -> Lazy.ByteString -> Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, a) 
runSimpleBinaryGet (BondGet g) = runGetOrFail g

runSimpleBinaryPut :: BondPut SimpleBinaryProto -> Lazy.ByteString
runSimpleBinaryPut (BondPut p) = runPut p
