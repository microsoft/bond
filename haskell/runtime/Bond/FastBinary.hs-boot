{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls, GADTs, MultiWayIf, InstanceSigs #-}
module Bond.FastBinary (
    deserializeFast,
    fastToStream
  ) where

import Bond.BinaryProto
import Bond.Stream
import Data.Int
import qualified Data.ByteString.Lazy as Lazy

data FastBinaryProto

deserializeFast :: forall a. BondStruct a => Lazy.ByteString -> Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, a) 

fastToStream :: BondGet FastBinaryProto StreamStruct
