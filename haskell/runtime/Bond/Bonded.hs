{-# Language FlexibleContexts, AllowAmbiguousTypes #-}
module Bond.Bonded (
        getBondedDecoder,
        unpackBonded
    ) where

import Bond.BinaryProto
import Bond.CompactBinary
--import Bond.FastBinary
--import Bond.SimpleBinary
import Bond.Types
import qualified Data.ByteString.Lazy as Lazy

unpackBonded :: (BondBinary CompactBinaryProto a,
                 BondBinary CompactBinaryV1Proto a
                ) => Bonded a -> Either String a
unpackBonded (BondedObject v) = Right v
unpackBonded (BondedStream s proto ver)
    = let decoder = getBondedDecoder proto ver
       in case decoder s of
            Right (rest, _, msg) | Lazy.null rest -> Right msg
            Right (_, _, _) -> Left "Not all input consumed"
            Left (_, _, msg) -> Left msg

getBondedDecoder :: (BondBinary CompactBinaryProto a,
                     BondBinary CompactBinaryV1Proto a
                    ) => Word16 -> Word16 -> BondedDecoder a
getBondedDecoder 0x4342 1 = runCompactBinaryV1Get bondGet
getBondedDecoder 0x4342 2 = runCompactBinaryGet bondGet

{-
parseContainer :: (BondBinary CompactBinaryV1Proto a, BondBinary CompactBinaryProto a) => BS.ByteString -> BondGet t a
parseContainer s = do
    let contsig = flip runGetOrFail s $ do
            sig <- getWord32be
            rest <- getRemainingLazyByteString
            return (sig, rest)
    parse <- case contsig of
        Left (_, _, msg) -> fail $ "error while parsing container: " ++ msg
        Right (_, _, (0x43420100, rest)) -> return $ runCompactBinaryV1Get bondGet rest
        Right (_, _, (0x43420200, rest)) -> return $ runCompactBinaryGet bondGet rest
        Right (_, _, (sig, _)) -> fail $ "unknown container signature " ++ show sig
    case parse of
        Right (r, _, v) | BS.null r -> return v
        Right _ -> fail "Not all container consumed"
        Left (_, _, msg) -> fail $ "error while parsing container: " ++ msg

-}
