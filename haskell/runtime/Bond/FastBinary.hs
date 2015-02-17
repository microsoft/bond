{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, MultiWayIf, MultiParamTypeClasses, FlexibleInstances, EmptyDataDecls #-}
module Bond.FastBinary (
    runFastBinaryGet,
    runFastBinaryPut
  ) where

import Bond.BinaryProto
import Bond.Types
import Bond.Wire
import Control.Applicative
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as Lazy

data FastBinaryProto

instance BondBinary FastBinaryProto Word16 where
    bondGet = BondGet getWord16le
    bondPut = BondPut . putWord16le

instance BondBinary FastBinaryProto Word32 where
    bondGet = BondGet getWord32le
    bondPut = BondPut . putWord32le

instance BondBinary FastBinaryProto Word64 where
    bondGet = BondGet getWord64le
    bondPut = BondPut . putWord64le

instance BondBinary FastBinaryProto Int16 where
    bondGet = BondGet (fromIntegral <$> getWord16le)
    bondPut = BondPut . putWord16le . fromIntegral

instance BondBinary FastBinaryProto Int32 where
    bondGet = BondGet (fromIntegral <$> getWord32le)
    bondPut = BondPut . putWord32le . fromIntegral

instance BondBinary FastBinaryProto Int64 where
    bondGet = BondGet (fromIntegral <$> getWord64le)
    bondPut = BondPut . putWord64le . fromIntegral

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

instance BondBinaryProto FastBinaryProto

runFastBinaryGet :: BondGet FastBinaryProto a -> Lazy.ByteString -> Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, a) 
runFastBinaryGet (BondGet g) = runGetOrFail g

runFastBinaryPut :: BondPut FastBinaryProto -> Lazy.ByteString
runFastBinaryPut (BondPut p) = runPut p
