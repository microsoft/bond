{-# Language FlexibleContexts, AllowAmbiguousTypes #-}
module Bond.Bonded (
        makeBonded,
        unpackBonded
    ) where

import Bond.BinaryProto
import Bond.CompactBinary
import Bond.FastBinary
import Bond.SimpleBinary
import Bond.Types
import qualified Data.ByteString.Lazy as Lazy

type Decoder a = Lazy.ByteString -> Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, a)

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

getDecoder :: BondStruct a => ProtoSig -> Decoder a
getDecoder proto
    | proto == compactV1Sig = deserializeCompactV1
    | proto == compactSig = deserializeCompact
    | proto == simpleV1Sig = deserializeSimpleV1
    | proto == simpleSig = deserializeSimple
    | proto == fastSig = deserializeFast
getDecoder _ = const $ Left (Lazy.empty, 0, "unknown protocol or version")
