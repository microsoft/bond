{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls, GADTs, MultiWayIf, InstanceSigs #-}
module Bond.CompactBinary (
    deserializeCompactV1,
    deserializeCompact,
    compactV1ToStream',
    compactToStream'
  ) where

import Bond.BinaryProto
import Bond.Stream
import Data.Int
import qualified Data.ByteString.Lazy as Lazy

deserializeCompactV1 :: forall a. BondStruct a => Lazy.ByteString -> Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, a) 

deserializeCompact :: forall a. BondStruct a => Lazy.ByteString -> Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, a) 

compactToStream' :: Lazy.ByteString -> StreamStruct

compactV1ToStream' :: Lazy.ByteString -> StreamStruct
