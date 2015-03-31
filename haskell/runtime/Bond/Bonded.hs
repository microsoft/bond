{-# Language FlexibleContexts, AllowAmbiguousTypes #-}
module Bond.Bonded (
        makeBonded,
        unpackBonded,
        streamBonded
    ) where

import Bond.BinaryProto
import {-# SOURCE #-} Bond.CompactBinary
import {-# SOURCE #-} Bond.FastBinary
import Bond.SimpleBinary
import Bond.Stream
import Bond.Types
import Data.Binary.Get
import qualified Data.ByteString.Lazy as Lazy

type BondDecoder a = Lazy.ByteString -> Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, a)

unpackBonded :: BondStruct a => Bonded a -> Either String a
unpackBonded (BondedObject v) = Right v
unpackBonded (BondedStream proto s)
    = let decoder = getDecoder proto
       in case decoder s of
            Right (rest, _, msg) | Lazy.null rest -> Right msg
            Right (_, _, _) -> Left "Not all input consumed"
            Left (_, _, msg) -> Left msg

makeBonded :: a -> Bonded a
makeBonded = BondedObject

streamBonded :: BondBinaryProto t => ProtoSig -> Lazy.ByteString -> BondPutM t StreamStruct
streamBonded sig s | sig == compactV1Sig = go compactV1ToStream
                   | sig == compactSig = go compactToStream
                   | sig == fastSig = go fastToStream
    where
    go f = let BondGet decoder = f
            in case runGetOrFail decoder s of
                Right (rest, _, msg) | Lazy.null rest -> return msg
                Right (_, _, _) -> fail "Not all input consumed"
                Left (_, _, msg) -> fail msg
streamBonded _ _ = error "internal error: unstreamable protocol"

getDecoder :: BondStruct a => ProtoSig -> BondDecoder a
getDecoder proto
    | proto == compactV1Sig = deserializeCompactV1
    | proto == compactSig = deserializeCompact
    | proto == simpleV1Sig = deserializeSimpleV1
    | proto == simpleSig = deserializeSimple
    | proto == fastSig = deserializeFast
getDecoder _ = const $ Left (Lazy.empty, 0, "unknown protocol or version")
