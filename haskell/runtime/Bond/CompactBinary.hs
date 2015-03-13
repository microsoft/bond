{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls, GADTs, MultiWayIf, InstanceSigs #-}
module Bond.CompactBinary (
    runCompactBinaryV1Get,
    runCompactBinaryV1Put,
--    runCompactBinaryGet,
--    runCompactBinaryPut,
    CompactBinaryV1Proto,
    CompactBinaryProto,
    ZigZagInt(..),     -- export for testing
    zigzagToWord,      -- export for testing
    wordToZigZag       -- export for testing
  ) where

import Bond.BinaryProto
import Bond.Cast
import Bond.Default
import Bond.Schema
import Bond.Types
import Bond.Wire
import Control.Applicative
import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.Maybe
import Data.Proxy
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.HashSet as H
import qualified Data.Map as M
import qualified Data.Vector as V

import Debug.Trace

checkWireType :: WireType a => Proxy a -> ItemType -> Bool
checkWireType p t = getWireType p == t

data CompactBinaryV1Proto
data CompactBinaryProto

newtype EncodedWord = EncodedWord { unWord :: Word64 }
newtype ZigZagInt = ZigZagInt { fromZigZag :: Int64 }
    deriving (Show, Eq)

zigzagToWord :: ZigZagInt -> EncodedWord
zigzagToWord (ZigZagInt i) | i >= 0 = EncodedWord $ 2 * fromIntegral i
zigzagToWord (ZigZagInt i) = EncodedWord $ (2 * fromIntegral (abs i)) - 1

wordToZigZag :: EncodedWord -> ZigZagInt
wordToZigZag (EncodedWord w) | even w = ZigZagInt $ fromIntegral (w `div` 2)
wordToZigZag (EncodedWord w) = ZigZagInt $ negate $ fromIntegral ((w - 1) `div` 2) + 1

putEncodedWord :: EncodedWord -> BondPut t
putEncodedWord (EncodedWord i) | i < 128 = BondPut $ putWord8 $ fromIntegral i
putEncodedWord (EncodedWord i) = do
    let iLow = fromIntegral $ i .&. 0x7F
    BondPut $ putWord8 $ iLow `setBit` 7
    putEncodedWord $ EncodedWord (i `shiftR` 7)

getEncodedWord :: BondGet t EncodedWord
getEncodedWord = EncodedWord <$> step 0
    where
    step :: Int -> BondGet t Word64
    step n | n > 9 = fail "EncodedWord: sequence too long"
    step n = do
        b <- fromIntegral <$> BondGet getWord8
        rest <- if b `testBit` 7 then step (n + 1)  else return (0 :: Word64)
        return $ (b `clearBit` 7) .|. (rest `shiftL` 7)

getVarInt :: BondGet t Int
getVarInt = BondGet $ step 0
    where
    step :: Int -> Get Int
    step n | n > 4 = fail "VarInt: sequence too long"
    step n = do
        b <- fromIntegral <$> getWord8
        rest <- if b `testBit` 7 then step (n + 1)  else return (0 :: Int)
        return $ (b `clearBit` 7) .|. (rest `shiftL` 7)

putVarInt :: Int -> BondPut t
putVarInt i | i < 0 = error "VarInt with negative value"
putVarInt i | i < 128 = BondPut $ putWord8 $ fromIntegral i
putVarInt i = let iLow = fromIntegral $ i .&. 0x7F
               in do
                    BondPut $ putWord8 $ iLow `setBit` 7
                    putVarInt (i `shiftR` 7)

typeIdOf :: forall a. WireType a => a -> Word8
typeIdOf _ =  fromIntegral $ fromEnum $ getWireType (Proxy :: Proxy a)

wireType :: Word8 -> ItemType
wireType = toEnum . fromIntegral

instance BondBinaryProto CompactBinaryV1Proto where
    bondPutBool True = BondPut $ putWord8 1
    bondPutBool False = BondPut $ putWord8 0
    bondPutUInt8 = BondPut . putWord8
    bondPutUInt16 = putEncodedWord . EncodedWord . fromIntegral
    bondPutUInt32 = putEncodedWord . EncodedWord . fromIntegral
    bondPutUInt64 = putEncodedWord . EncodedWord . fromIntegral
    bondPutInt8 = BondPut . putWord8 . fromIntegral
    bondPutInt16 = putEncodedWord . zigzagToWord . ZigZagInt . fromIntegral
    bondPutInt32 = putEncodedWord . zigzagToWord . ZigZagInt . fromIntegral
    bondPutInt64 = putEncodedWord . zigzagToWord . ZigZagInt . fromIntegral
    bondPutFloat = BondPut . putWord32le . floatToWord
    bondPutDouble = BondPut . putWord64le . doubleToWord
    bondPutString (Utf8 s) = do
        putVarInt $ fromIntegral $ BS.length s
        BondPut $ putByteString s
    bondPutWString (Utf16 s) = do
        putVarInt $ fromIntegral $ BS.length s `div` 2
        BondPut $ putByteString s
    bondPutList xs = do
        BondPut $ putWord8 $ typeIdOf (head xs)
        putVarInt $ length xs
        mapM_ bondPut xs
    bondPutNullable = bondPutList . maybeToList
    bondPutSet = bondPutList . H.toList
    bondPutMap m = do
        BondPut $ putWord8 $ typeIdOf (head $ M.keys m)
        BondPut $ putWord8 $ typeIdOf (head $ M.elems m)
        putVarInt $ M.size m
        forM_ (M.toList m) $ \(k, v) -> do
            bondPut k
            bondPut v
    bondPutVector xs = do
        BondPut $ putWord8 $ typeIdOf (V.head xs)
        putVarInt $ V.length xs
        V.mapM_ bondPut xs
    bondPutBlob (Blob b) = do
        BondPut $ putWord8 $ fromIntegral $ fromEnum BT_INT8
        putVarInt $ BS.length b
        BondPut $ putByteString b
    bondPutBonded (BondedObject a) = bondPut a
    bondPutBonded (BondedStream sig s) | sig == compactV1Sig = BondPut $ putLazyByteString s
    bondPutStruct = putStructV1 BT_STOP (bondGetInfo Proxy)

    bondGetBool = do
        v <- BondGet getWord8
        return $ Just $ v /= 0
    bondGetUInt8 = Just <$> BondGet getWord8
    bondGetUInt16 = Just . fromIntegral . unWord <$> getEncodedWord
    bondGetUInt32 = Just . fromIntegral . unWord <$> getEncodedWord
    bondGetUInt64 = Just . fromIntegral . unWord <$> getEncodedWord
    bondGetInt8 = Just . fromIntegral <$> BondGet getWord8
    bondGetInt16 = Just . fromIntegral . fromZigZag . wordToZigZag <$> getEncodedWord
    bondGetInt32 = Just . fromIntegral . fromZigZag . wordToZigZag <$> getEncodedWord
    bondGetInt64 = Just . fromIntegral . fromZigZag . wordToZigZag <$> getEncodedWord
    bondGetFloat = Just . wordToFloat <$> BondGet getWord32le
    bondGetDouble = Just . wordToDouble <$> BondGet getWord64le
    bondGetString = do
        n <- getVarInt
        Just . Utf8 <$> BondGet (getByteString n)
    bondGetWString = do
        n <- getVarInt
        Just . Utf16 <$> BondGet (getByteString $ n * 2)
    bondGetBlob = do
        z <- BondGet bytesRead
        traceShowM("blob", z)
        tag <- wireType <$> BondGet getWord8
        n <- getVarInt
        if tag == BT_INT8
            then Just . Blob <$> BondGet (getByteString n)
            else do
                replicateM_ n (skipValue tag)
                return Nothing
    bondGetList = getListV1
    bondGetSet = liftM H.fromList <$> getListV1
    bondGetMap = getMap
    bondGetVector = liftM V.fromList <$> getListV1
    bondGetNullable = do
        v <- getListV1
        return $ case v of
            Just [x] -> Just $ Just x
            Just [] -> Just Nothing
            _ -> Nothing
    bondGetBonded :: forall a. BondBinary a => BondGet CompactBinaryV1Proto (Maybe (Bonded a))
    bondGetBonded = do
        let try (BondGet g) = BondGet $ lookAhead g
        size <- try $ do
            start <- BondGet bytesRead
            skipValue (getWireType (Proxy :: Proxy a))
            end <- BondGet bytesRead
            return (end - start)
        Just . BondedStream compactV1Sig <$> BondGet (getLazyByteString size)
    bondGetStruct = Just <$> getStructV1 (bondGetInfo Proxy)

getMap :: forall a b t. (BondBinaryProto t, Ord a, BondBinary a, BondBinary b) => BondGet t (Maybe (M.Map a b))
getMap = do
    z <- BondGet bytesRead
    traceShowM("map", z)
    ktag <- wireType <$> BondGet getWord8
    vtag <- wireType <$> BondGet getWord8
    n <- getVarInt
    if checkWireType (Proxy :: Proxy a) ktag && checkWireType (Proxy :: Proxy b) vtag
        then do
            elems <- replicateM n $ do
                k <- bondGet
                v <- bondGet
                return $ seqPair (k, v)
            return $ M.fromList <$> sequence elems
        else do
            replicateM_ n $ do
                skipValue ktag
                skipValue vtag
            return Nothing
    where
    seqPair (Just a, Just b) = Just (a, b)
    seqPair _ = Nothing

getListV1 :: forall a. BondBinary a => BondGet CompactBinaryV1Proto (Maybe [a])
getListV1 = do
    z <- BondGet bytesRead
    traceShowM("list", z)
    tag <- wireType <$> BondGet getWord8
    n <- getVarInt
    if checkWireType (Proxy :: Proxy a) tag
        then do
            elems <- replicateM n bondGet
            return $ sequence elems
        else do
            replicateM_ n (skipValue tag)
            return Nothing

getFieldHeader :: BondGet t (ItemType, Ordinal)
getFieldHeader = do
    z <- BondGet bytesRead
    tag <- BondGet getWord8
    traceShowM ("field", z, tag)
    case tag `shiftR` 5 of
        6 -> do
            n <- BondGet getWord8
            return (wireType (tag .&. 31), Ordinal (fromIntegral n))
        7 -> do
            n <- BondGet getWord16le
            return (wireType (tag .&. 31), Ordinal n)
        n -> return (wireType (tag .&. 31), Ordinal (fromIntegral n))

getStructV1 :: forall a b. StructInfo a b -> BondGet CompactBinaryV1Proto a
getStructV1 (StructInfo _ pb) = do
    def <- case pb of
        Nothing -> return defaultValue
        Just p -> do
            base <- getStructV1 (bondGetInfo p)
            return $ bondSetBase defaultValue base
    getFields def

getFields :: forall a. BondStruct a => a -> BondGet CompactBinaryV1Proto a
getFields def = do
    update <- loop id
    return $ update def
    where
    try :: BondGet t (Maybe (b -> b)) -> BondGet t (Maybe (b -> b))
    try (BondGet g) = BondGet $ lookAheadM g
    loop f = do
        (t, n) <- getFieldHeader
        case t of
            BT_STOP -> return f
            BT_STOP_BASE -> return f -- XXX think later
            _ -> do
                fieldMod <- try $ bondSetField n t
                case fieldMod of
                    Just func -> loop (func . f)
                    Nothing -> do
                        skipValue t
                        loop f

putStructV1 :: ItemType -> StructInfo a b -> a -> BondPut CompactBinaryV1Proto
putStructV1 stop (StructInfo pa pb) a = do
    when (isJust pb) $ putStructV1 BT_STOP_BASE (bondGetInfo $ fromJust pb) (bondGetBase a)
    putFields (bondGetSchema pa)
    BondPut $ putWord8 $ fromIntegral $ fromEnum stop
    where
    putFields (StructSchema fields) = forM_ fields $ \(FieldInfo _ t n) -> unless (bondFieldHasDefaultValue a n) $ do
        putFieldHeader t n
        bondPutField a n
    putFieldHeader t (Ordinal n) = let tbits = fromIntegral $ fromEnum t
                                       nbits = fromIntegral n
                                    in if | n <= 5 -> BondPut $ putWord8 $ tbits .|. (nbits `shiftL` 5)
                                          | n <= 255 -> do
                                                    BondPut $ putWord8 $ tbits .|. 0xC0
                                                    BondPut $ putWord8 nbits
                                          | otherwise -> do
                                                    BondPut $ putWord8 $ tbits .|. 0xE0
                                                    BondPut $ putWord16le n

skipVarInt :: BondGet t ()
skipVarInt = loop
    where
    loop = do
        v <- BondGet getWord8
        when (v `testBit` 7) loop

skipValue :: ItemType -> BondGet t ()
skipValue BT_STOP = fail "internal error: skipValue BT_STOP"
skipValue BT_STOP_BASE = fail "internal error: skipValue BT_STOP_BASE"
skipValue BT_BOOL = BondGet $ skip 1
skipValue BT_UINT8 = BondGet $ skip 1
skipValue BT_UINT16 = skipVarInt
skipValue BT_UINT32 = skipVarInt
skipValue BT_UINT64 = skipVarInt
skipValue BT_FLOAT = BondGet $ skip 4
skipValue BT_DOUBLE = BondGet $ skip 8
skipValue BT_INT8 = BondGet $ skip 1
skipValue BT_INT16 = skipVarInt
skipValue BT_INT32 = skipVarInt
skipValue BT_INT64 = skipVarInt
skipValue BT_STRING = do
    n <- getVarInt
    BondGet $ skip n
skipValue BT_WSTRING = do
    n <- getVarInt
    BondGet $ skip (n * 2)
skipValue BT_LIST = do
    z <- BondGet bytesRead
    traceShowM("skip list", z)
    t <- wireType <$> BondGet getWord8
    n <- getVarInt
    replicateM_ n (skipValue t)
skipValue BT_SET = skipValue BT_LIST
skipValue BT_MAP = do
    z <- BondGet bytesRead
    traceShowM("skip map", z)
    tkey <- wireType <$> BondGet getWord8
    tvalue <- wireType <$> BondGet getWord8
    n <- getVarInt
    replicateM_ n $ do
        skipValue tkey
        skipValue tvalue
skipValue BT_STRUCT = loop
    where
    loop = do
        (t, _) <- getFieldHeader
        case t of
            BT_STOP -> return ()
            BT_STOP_BASE -> loop
            _ -> do
                    skipValue t
                    loop

{-
instance BondBinaryProto CompactBinaryProto where
    bondPutBool True = BondPut $ putWord8 1
    bondPutBool False = BondPut $ putWord8 0
    bondPutUInt8 = BondPut . putWord8
    bondPutUInt16 = putEncodedWord . EncodedWord . fromIntegral
    bondPutUInt32 = putEncodedWord . EncodedWord . fromIntegral
    bondPutUInt64 = putEncodedWord . EncodedWord . fromIntegral
    bondPutInt8 = BondPut . putWord8 . fromIntegral
    bondPutInt16 = putEncodedWord . zigzagToWord . ZigZagInt . fromIntegral
    bondPutInt32 = putEncodedWord . zigzagToWord . ZigZagInt . fromIntegral
    bondPutInt64 = putEncodedWord . zigzagToWord . ZigZagInt . fromIntegral
    bondPutFloat = BondPut . putWord32le . floatToWord
    bondPutDouble = BondPut . putWord64le . doubleToWord
    bondPutString (Utf8 s) = do
        putVarInt $ fromIntegral $ BS.length s
        BondPut $ putByteString s
    bondPutWString (Utf16 s) = do
        putVarInt $ fromIntegral $ BS.length s `div` 2
        BondPut $ putByteString s
    bondPutList xs = do
        let t = typeIdOf (head xs)
        if length xs < 7
            then BondPut $ putWord8 $ t .|. fromIntegral ((1 + length xs) `shiftL` 5)
            else do
                BondPut $ putWord8 t
                putVarInt $ length xs
        mapM_ bondPut xs

    bondGetBool = do
        v <- BondGet getWord8
        return $ Just $ v /= 0
    bondGetUInt8 = Just <$> BondGet getWord8
    bondGetUInt16 = Just . fromIntegral . unWord <$> getEncodedWord
    bondGetUInt32 = Just . fromIntegral . unWord <$> getEncodedWord
    bondGetUInt64 = Just . fromIntegral . unWord <$> getEncodedWord
    bondGetInt8 = Just . fromIntegral <$> BondGet getWord8
    bondGetInt16 = Just . fromIntegral . fromZigZag . wordToZigZag <$> getEncodedWord
    bondGetInt32 = Just . fromIntegral . fromZigZag . wordToZigZag <$> getEncodedWord
    bondGetInt64 = Just . fromIntegral . fromZigZag . wordToZigZag <$> getEncodedWord
    bondGetFloat = Just . wordToFloat <$> BondGet getWord32le
    bondGetDouble = Just . wordToDouble <$> BondGet getWord64le
    bondGetString = do
        n <- getVarInt
        Just . Utf8 <$> BondGet (getByteString n)
    bondGetWString = do
        n <- getVarInt
        Just . Utf16 <$> BondGet (getByteString $ n * 2)
    bondGetList = getListV2
    
getListV2 :: forall a. BondBinary a => BondGet CompactBinaryProto (Maybe [a])
getListV2 = do
    v <- BondGet getWord8
    (t, n) <- if v `shiftR` 5 /= 0
                then return (wireType (v .&. 0x1f), fromIntegral (v `shiftR` 5) - 1)
                else do
                    num <- getVarInt
                    return (wireType v, num)
    if checkWireType (Proxy :: Proxy a) t
        then do
            elems <- replicateM n bondGet
            return $ sequence elems
        else do
            -- FIXME skip n elems by type
            return Nothing

    readStruct reader = do
        VarInt _ <- bondGet -- FIXME use "isolate" from Data.Binary >= 0.7.2.0
        reader
    putStruct (BondPut writer) = do
        let bs = runPut writer
        bondPut $ VarInt $ fromIntegral $ Lazy.length bs
        BondPut $ putLazyByteString bs
    skipValue = skipCompactV2Value
    protoSignature = const compactSig

skipCompactValue :: (BondBinaryProto t, BondBinary t ListHead) => BondBinary t FieldTag => ItemType -> BondGet t ()
skipCompactValue BT_UINT16 = skipVarInt
skipCompactValue BT_UINT32 = skipVarInt
skipCompactValue BT_UINT64 = skipVarInt
skipCompactValue BT_INT16 = skipVarInt
skipCompactValue BT_INT32 = skipVarInt
skipCompactValue BT_INT64 = skipVarInt
skipCompactValue t = skipBinaryValue t

skipCompactV2Value :: BondBinary CompactBinaryProto FieldTag => ItemType -> BondGet CompactBinaryProto ()
skipCompactV2Value BT_STRUCT = do
    VarInt len <- bondGet
    BondGet $ skip len
skipCompactV2Value t = skipCompactValue t
-}

runCompactBinaryV1Get :: forall a. BondStruct a => Lazy.ByteString -> Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, a) 
runCompactBinaryV1Get = let BondGet g = bondGet :: BondGet CompactBinaryV1Proto (Maybe a)
                         in runGetOrFail (fromJust <$> g)

runCompactBinaryV1Put :: BondPut CompactBinaryV1Proto -> Lazy.ByteString
runCompactBinaryV1Put (BondPut p) = runPut p

{-
runCompactBinaryGet :: forall a. BondStruct a => Lazy.ByteString -> Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, a) 
runCompactBinaryGet = let BondGet g = bondGet :: BondGet CompactBinaryProto (Maybe a)
                       in runGetOrFail (fromJust <$> g)

runCompactBinaryPut :: BondPut CompactBinaryProto -> Lazy.ByteString
runCompactBinaryPut (BondPut p) = runPut p
-}
