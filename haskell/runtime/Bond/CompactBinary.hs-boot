{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls, GADTs #-}
module Bond.CompactBinary (
    deserializeCompactV1,
    deserializeCompact,
    compactV1ToStream,
    compactToStream
  ) where

import Bond.BinaryProto
import Bond.Stream
import Data.Int
import qualified Data.ByteString.Lazy as Lazy

data CompactBinaryV1Proto
data CompactBinaryProto

deserializeCompactV1 :: forall a. BondStruct a => Lazy.ByteString -> Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, a) 

deserializeCompact :: forall a. BondStruct a => Lazy.ByteString -> Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, a) 

compactToStream :: BondGet CompactBinaryProto StreamStruct
compactV1ToStream :: BondGet CompactBinaryV1Proto StreamStruct
