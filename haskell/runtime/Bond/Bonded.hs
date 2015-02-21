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

unpackBonded :: (BondBinary CompactBinaryProto a,
                 BondBinary CompactBinaryV1Proto a,
                 BondBinary FastBinaryProto a,
                 BondBinary SimpleBinaryProto a,
                 BondBinary SimpleBinaryV1Proto a
                ) => Bonded a -> Either String a
unpackBonded (BondedObject v) = Right v
unpackBonded (BondedStream s proto ver)
    = let decoder = getDecoder proto ver
       in case decoder s of
            Right (rest, _, msg) | Lazy.null rest -> Right msg
            Right (_, _, _) -> Left "Not all input consumed"
            Left (_, _, msg) -> Left msg

makeBonded :: a -> Bonded a
makeBonded = BondedObject

getDecoder :: (BondBinary CompactBinaryProto a,
               BondBinary CompactBinaryV1Proto a,
               BondBinary FastBinaryProto a,
               BondBinary SimpleBinaryProto a,
               BondBinary SimpleBinaryV1Proto a
              ) => ProtoSig -> Word16 -> Decoder a
getDecoder proto ver
    | proto == compactSig && ver == 1 = runCompactBinaryV1Get bondGet
    | proto == compactSig && ver == 2 = runCompactBinaryGet bondGet
    | proto == simpleSig && ver == 1 = runSimpleBinaryV1Get bondGet
    | proto == simpleSig && ver == 2 = runSimpleBinaryGet bondGet
    | proto == fastSig && ver == 1 = runFastBinaryGet bondGet
getDecoder _ _ = const $ Left (Lazy.empty, 0, "unknown protocol or version")
