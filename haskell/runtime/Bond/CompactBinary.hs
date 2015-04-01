{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls, GADTs #-}
module Bond.CompactBinary (
    deserializeCompactV1,
    serializeCompactV1,
    deserializeCompact,
    serializeCompact,
    compactV1ToStream,
    streamToCompactV1,
    compactToStream,
    streamToCompact,
    CompactBinaryV1Proto,
    CompactBinaryProto,
    ZigZagInt(..),     -- export for testing
    zigzagToWord,      -- export for testing
    wordToZigZag       -- export for testing
  ) where

import Bond.BinaryProto
import Bond.Bonded
import Bond.Cast
import Bond.Default
import Bond.Schema
import Bond.Stream
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

checkWireType :: WireType a => Proxy a -> ItemType -> Bool
checkWireType p t = getWireType p == t

data CompactBinaryV1Proto
data CompactBinaryProto

class BondBinaryProto t => Skippable t where
    skipValue :: ItemType -> BondGet t ()

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

wireTypeOf :: forall a. WireType a => a -> ItemType
wireTypeOf _ =  getWireType (Proxy :: Proxy a)

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
    bondPutMaybe Nothing = return ()
    bondPutMaybe (Just v) = bondPut v
    bondPutNullable = bondPutList . maybeToList
    bondPutSet = bondPutList . H.toList
    bondPutMap = putMap
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
    bondPutBonded b@(BondedStream sig _) | sig == simpleV1Sig || sig == simpleSig = do
        let ret = unpackBonded b
        case ret of
            Left err -> fail err
            Right a -> bondPut a
    bondPutBonded (BondedStream sig s) = streamBonded sig s >>= streamToCompactV1 
    bondPutStruct = saveStruct BT_STOP (bondGetInfo Proxy)

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
    bondGetBonded = getBonded compactV1Sig
    bondGetStruct = Just <$> readStruct (bondGetInfo Proxy)

instance Skippable CompactBinaryV1Proto where
    skipValue = skipValueV1

skipValueV1 :: ItemType -> BondGet CompactBinaryV1Proto ()
skipValueV1 = skipValueCommon

getBonded :: forall a t. (Skippable t, BondBinaryProto t, BondStruct a) => ProtoSig -> BondGet t (Maybe (Bonded a))
getBonded sig = do
    let try (BondGet g) = BondGet $ lookAhead g
    size <- try $ do
        start <- BondGet bytesRead
        skipValue (getWireType (Proxy :: Proxy a)) :: BondGet t ()
        end <- BondGet bytesRead
        return (end - start)
    Just . BondedStream sig <$> BondGet (getLazyByteString size)

getMap :: forall a b t. (Skippable t, BondBinaryProto t, Ord a, BondBinary a, BondBinary b) => BondGet t (Maybe (M.Map a b))
getMap = do
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

putMap :: (BondBinaryProto t, BondBinary a, BondBinary b) => M.Map a b -> BondPut t
putMap m = do
    BondPut $ putWord8 $ typeIdOf (head $ M.keys m)
    BondPut $ putWord8 $ typeIdOf (head $ M.elems m)
    putVarInt $ M.size m
    forM_ (M.toList m) $ \(k, v) -> do
        bondPut k
        bondPut v

getListV1 :: forall a. BondBinary a => BondGet CompactBinaryV1Proto (Maybe [a])
getListV1 = do
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
    tag <- BondGet getWord8
    case tag `shiftR` 5 of
        6 -> do
            n <- BondGet getWord8
            return (wireType (tag .&. 31), Ordinal (fromIntegral n))
        7 -> do
            n <- BondGet getWord16le
            return (wireType (tag .&. 31), Ordinal n)
        n -> return (wireType (tag .&. 31), Ordinal (fromIntegral n))

putFieldHeader :: ItemType -> Ordinal -> BondPut t
putFieldHeader t (Ordinal n) = let tbits = fromIntegral $ fromEnum t
                                   nbits = fromIntegral n
                                in case () of
                                    _ |n <= 5 -> BondPut $ putWord8 $ tbits .|. (nbits `shiftL` 5)
                                      | n <= 255 -> do
                                                BondPut $ putWord8 $ tbits .|. 0xC0
                                                BondPut $ putWord8 nbits
                                      | otherwise -> do
                                                BondPut $ putWord8 $ tbits .|. 0xE0
                                                BondPut $ putWord16le n

readStruct :: forall a b t. (Skippable t, BondBinaryProto t) => StructInfo a b -> BondGet t a
readStruct (StructInfo _ pb) = do
    def <- case pb of
        Nothing -> return defaultValue
        Just p -> do
            base <- readStruct (bondGetInfo p)
            return $ bondSetBase defaultValue base
    getFields def

getFields :: forall a t. (BondStruct a, Skippable t, BondBinaryProto t) => a -> BondGet t a
getFields def = do
    update <- loop id
    return $ update def
    where
    try :: BondGet t (Maybe (b -> b)) -> BondGet t (Maybe (b -> b))
    try (BondGet g) = BondGet $ lookAheadM g
    loop f = do
        (tag, n) <- getFieldHeader
        case tag of
            BT_STOP -> return f
            BT_STOP_BASE -> return f -- XXX think later
            _ -> do
                fieldMod <- try $ bondSetField n tag
                case fieldMod of
                    Just func -> loop (func . f)
                    Nothing -> do
                        skipValue tag
                        loop f

saveStruct :: BondBinaryProto t => ItemType -> StructInfo a b -> a -> BondPut t
saveStruct stop (StructInfo pa pb) a = do
    when (isJust pb) $ saveStruct BT_STOP_BASE (bondGetInfo $ fromJust pb) (bondGetBase a)
    putFields (bondGetSchema pa)
    BondPut $ putWord8 $ fromIntegral $ fromEnum stop
    where
    putFields (StructSchema fields) = forM_ fields $ \(FieldInfo _ t n) -> unless (bondFieldHasDefaultValue a n) $ do
        putFieldHeader t n
        bondPutField a n

skipVarInt :: BondGet t ()
skipVarInt = loop
    where
    loop = do
        v <- BondGet getWord8
        when (v `testBit` 7) loop

skipValueCommon :: Skippable t => ItemType -> BondGet t ()
skipValueCommon BT_STOP = fail "internal error: skipValue BT_STOP"
skipValueCommon BT_STOP_BASE = fail "internal error: skipValue BT_STOP_BASE"
skipValueCommon BT_BOOL = BondGet $ skip 1
skipValueCommon BT_UINT8 = BondGet $ skip 1
skipValueCommon BT_UINT16 = skipVarInt
skipValueCommon BT_UINT32 = skipVarInt
skipValueCommon BT_UINT64 = skipVarInt
skipValueCommon BT_FLOAT = BondGet $ skip 4
skipValueCommon BT_DOUBLE = BondGet $ skip 8
skipValueCommon BT_INT8 = BondGet $ skip 1
skipValueCommon BT_INT16 = skipVarInt
skipValueCommon BT_INT32 = skipVarInt
skipValueCommon BT_INT64 = skipVarInt
skipValueCommon BT_STRING = do
    n <- getVarInt
    BondGet $ skip n
skipValueCommon BT_WSTRING = do
    n <- getVarInt
    BondGet $ skip (n * 2)
skipValueCommon BT_LIST = do
    t <- wireType <$> BondGet getWord8
    n <- getVarInt
    replicateM_ n (skipValue t)
skipValueCommon BT_SET = skipValue BT_LIST
skipValueCommon BT_MAP = do
    tkey <- wireType <$> BondGet getWord8
    tvalue <- wireType <$> BondGet getWord8
    n <- getVarInt
    replicateM_ n $ do
        skipValue tkey
        skipValue tvalue
skipValueCommon BT_STRUCT = loop
    where
    loop = do
        (t, _) <- getFieldHeader
        case t of
            BT_STOP -> return ()
            BT_STOP_BASE -> loop
            _ -> do
                    skipValue t
                    loop

deserializeCompactV1 :: forall a. BondStruct a => Lazy.ByteString -> Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, a) 
deserializeCompactV1 = let BondGet g = bondGet :: BondGet CompactBinaryV1Proto (Maybe a)
                        in runGetOrFail (fromJust <$> g)

serializeCompactV1 :: BondStruct a => a -> Lazy.ByteString
serializeCompactV1 v = let BondPut g = bondPut v :: BondPut CompactBinaryV1Proto
                        in runPut g

compactV1ToStream :: BondGet CompactBinaryV1Proto StreamStruct
compactV1ToStream = loop (StreamStruct Nothing [])
    where
    getElem t = do
      case t of
        BT_STOP -> error "internal error: getElem BT_STOP"
        BT_STOP_BASE -> error "internal error: getElem BT_STOP_BASE"
        BT_BOOL -> do
            Just v <- bondGetBool
            return $ SeBool v
        BT_UINT8 -> do
            Just v <- bondGetUInt8
            return $ SeUInt8 v
        BT_UINT16 -> do
            Just v <- bondGetUInt16
            return $ SeUInt16 v
        BT_UINT32 -> do
            Just v <- bondGetUInt32
            return $ SeUInt32 v
        BT_UINT64 -> do
            Just v <- bondGetUInt64
            return $ SeUInt64 v
        BT_FLOAT -> do
            Just v <- bondGetFloat
            return $ SeFloat v
        BT_DOUBLE -> do
            Just v <- bondGetDouble
            return $ SeDouble v
        BT_STRING -> do
            Just v <- bondGetString
            return $ SeString v
        BT_STRUCT -> do
            s <- compactV1ToStream
            return $ SeStruct s
        BT_LIST -> do
            tag <- wireType <$> BondGet getWord8
            n <- getVarInt
            elems <- replicateM n (getElem tag)
            return $ SeList tag elems
        BT_SET -> do
            tag <- wireType <$> BondGet getWord8
            n <- getVarInt
            elems <- replicateM n (getElem tag)
            return $ SeSet tag elems
        BT_MAP -> do
            ktag <- wireType <$> BondGet getWord8
            vtag <- wireType <$> BondGet getWord8
            n <- getVarInt
            elems <- replicateM n $ do
                k <- getElem ktag
                v <- getElem vtag
                return (k, v)
            return $ SeMap (ktag, vtag) elems
        BT_INT8 -> do
            Just v <- bondGetInt8
            return $ SeInt8 v
        BT_INT16 -> do
            Just v <- bondGetInt16
            return $ SeInt16 v
        BT_INT32 -> do
            Just v <- bondGetInt32
            return $ SeInt32 v
        BT_INT64 -> do
            Just v <- bondGetInt64
            return $ SeInt64 v
        BT_WSTRING -> do
            Just v <- bondGetWString
            return $ SeWString v
    loop st = do
        (tag, n) <- getFieldHeader
        case tag of
            BT_STOP -> return st
            BT_STOP_BASE -> loop (StreamStruct (Just st) [])
            _ -> do
                e <- getElem tag
                loop $ st { ssElems = ssElems st ++ [(n, e)] }

streamToCompactV1 :: StreamStruct -> BondPut CompactBinaryV1Proto
streamToCompactV1 struct = do
    putStreamStruct struct
    BondPut $ putWord8 $ fromIntegral $ fromEnum BT_STOP
    where
    putStreamStruct st = do
        when (isJust $ ssBase st) $ do
            putStreamStruct (fromJust $ ssBase st)
            BondPut $ putWord8 $ fromIntegral $ fromEnum BT_STOP_BASE
        mapM_ putField (ssElems st)
    putField (n, e) = do
        putFieldHeader (elemItemType e) n
        putElem e
    putElem (SeBool v) = bondPutBool v
    putElem (SeUInt8 v) = bondPutUInt8 v
    putElem (SeUInt16 v) = bondPutUInt16 v
    putElem (SeUInt32 v) = bondPutUInt32 v
    putElem (SeUInt64 v) = bondPutUInt64 v
    putElem (SeFloat v) = bondPutFloat v
    putElem (SeDouble v) = bondPutDouble v
    putElem (SeString v) = bondPutString v
    putElem (SeStruct v) = streamToCompactV1 v
    putElem (SeList tag xs) = do
        BondPut $ putWord8 $ fromIntegral $ fromEnum tag
        putVarInt $ length xs
        mapM_ putElem xs
    putElem (SeSet tag xs) = do
        BondPut $ putWord8 $ fromIntegral $ fromEnum tag
        putVarInt $ length xs
        mapM_ putElem xs
    putElem (SeMap (ktag, vtag) xs) = do
        BondPut $ putWord8 $ fromIntegral $ fromEnum ktag
        BondPut $ putWord8 $ fromIntegral $ fromEnum vtag
        putVarInt $ length xs
        forM_ xs $ \(k, v) -> do
            putElem k
            putElem v
    putElem (SeInt8 v) = do
        bondPutInt8 v
    putElem (SeInt16 v) = do
        bondPutInt16 v
    putElem (SeInt32 v) = do
        bondPutInt32 v
    putElem (SeInt64 v) = do
        bondPutInt64 v
    putElem (SeWString v) = do
        bondPutWString v

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
        putV2ListHeader (wireTypeOf $ head xs) (length xs)
        mapM_ bondPut xs
    bondPutMaybe Nothing = return ()
    bondPutMaybe (Just v) = bondPut v
    bondPutNullable = bondPutList . maybeToList
    bondPutSet = bondPutList . H.toList
    bondPutMap = putMap
    bondPutVector xs = do
        putV2ListHeader (wireTypeOf $ V.head xs) (V.length xs)
        V.mapM_ bondPut xs
    bondPutBlob (Blob b) = do
        putV2ListHeader BT_INT8 (BS.length b)
        BondPut $ putByteString b
    bondPutBonded (BondedObject a) = bondPut a
    bondPutBonded (BondedStream sig s) | sig == compactSig = BondPut $ putLazyByteString s
    bondPutBonded b@(BondedStream sig _) | sig == simpleV1Sig || sig == simpleSig = do
        let ret = unpackBonded b
        case ret of
            Left err -> fail err
            Right a -> bondPut a
    bondPutBonded (BondedStream sig s) = streamBonded sig s >>= streamToCompact 
    bondPutStruct = putStruct (bondGetInfo Proxy)

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
        (tag, n) <- getV2ListHeader
        if tag == BT_INT8
            then Just . Blob <$> BondGet (getByteString n)
            else do
                replicateM_ n (skipValue tag)
                return Nothing
    bondGetList = getList
    bondGetSet = liftM H.fromList <$> getList
    bondGetMap = getMap
    bondGetVector = liftM V.fromList <$> getList
    bondGetNullable = do
        v <- getList
        return $ case v of
            Just [x] -> Just $ Just x
            Just [] -> Just Nothing
            _ -> Nothing
    bondGetBonded = getBonded compactSig
    bondGetStruct = Just <$> getStruct (bondGetInfo Proxy)

instance Skippable CompactBinaryProto where
    skipValue = skipValueV2

getV2ListHeader :: BondGet CompactBinaryProto (ItemType, Int)
getV2ListHeader = do
    v <- BondGet getWord8
    if v `shiftR` 5 /= 0
        then return (wireType (v .&. 0x1f), fromIntegral (v `shiftR` 5) - 1)
        else do
                num <- getVarInt
                return (wireType v, num)

putV2ListHeader :: ItemType -> Int -> BondPut CompactBinaryProto
putV2ListHeader t n = do
    let tag = fromIntegral $ fromEnum t
    if n < 7
        then BondPut $ putWord8 $ tag .|. fromIntegral ((1 + n) `shiftL` 5)
        else do
            BondPut $ putWord8 tag
            putVarInt n

skipValueV2 :: ItemType -> BondGet CompactBinaryProto ()
skipValueV2 BT_LIST = do
    (tag, n) <- getV2ListHeader
    replicateM_ n (skipValue tag)
skipValueV2 BT_STRUCT = do
    len <- getVarInt
    BondGet $ skip len
skipValueV2 t = skipValueCommon t

getList :: forall a. BondBinary a => BondGet CompactBinaryProto (Maybe [a])
getList = do
    (tag, n) <- getV2ListHeader
    if checkWireType (Proxy :: Proxy a) tag
        then do
            elems <- replicateM n bondGet
            return $ sequence elems
        else do
            replicateM_ n (skipValue tag)
            return Nothing

putStruct :: StructInfo a b -> a -> BondPut CompactBinaryProto
putStruct p v = do
    let BondPut writer = saveStruct BT_STOP p v :: BondPut CompactBinaryProto
    let bs = runPut writer
    putVarInt $ fromIntegral $ Lazy.length bs
    BondPut $ putLazyByteString bs

getStruct :: forall a b. StructInfo a b -> BondGet CompactBinaryProto a
getStruct p = do
    void getVarInt -- FIXME use "isolate" from Data.Binary >= 0.7.2.0
    readStruct p

deserializeCompact :: forall a. BondStruct a => Lazy.ByteString -> Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, a) 
deserializeCompact = let BondGet g = bondGet :: BondGet CompactBinaryProto (Maybe a)
                      in runGetOrFail (fromJust <$> g)

serializeCompact :: BondStruct a => a -> Lazy.ByteString
serializeCompact v = let BondPut g = bondPut v :: BondPut CompactBinaryProto
                      in runPut g

compactToStream :: BondGet CompactBinaryProto StreamStruct
compactToStream = do
    void getVarInt
    loop (StreamStruct Nothing [])
    where
    getElem t = do
      case t of
        BT_STOP -> error "internal error: getElem BT_STOP"
        BT_STOP_BASE -> error "internal error: getElem BT_STOP_BASE"
        BT_BOOL -> do
            Just v <- bondGetBool
            return $ SeBool v
        BT_UINT8 -> do
            Just v <- bondGetUInt8
            return $ SeUInt8 v
        BT_UINT16 -> do
            Just v <- bondGetUInt16
            return $ SeUInt16 v
        BT_UINT32 -> do
            Just v <- bondGetUInt32
            return $ SeUInt32 v
        BT_UINT64 -> do
            Just v <- bondGetUInt64
            return $ SeUInt64 v
        BT_FLOAT -> do
            Just v <- bondGetFloat
            return $ SeFloat v
        BT_DOUBLE -> do
            Just v <- bondGetDouble
            return $ SeDouble v
        BT_STRING -> do
            Just v <- bondGetString
            return $ SeString v
        BT_STRUCT -> do
            s <- compactToStream
            return $ SeStruct s
        BT_LIST -> do
            (tag, n) <- getV2ListHeader
            elems <- replicateM n (getElem tag)
            return $ SeList tag elems
        BT_SET -> do
            (tag, n) <- getV2ListHeader
            elems <- replicateM n (getElem tag)
            return $ SeSet tag elems
        BT_MAP -> do
            ktag <- wireType <$> BondGet getWord8
            vtag <- wireType <$> BondGet getWord8
            n <- getVarInt
            elems <- replicateM n $ do
                k <- getElem ktag
                v <- getElem vtag
                return (k, v)
            return $ SeMap (ktag, vtag) elems
        BT_INT8 -> do
            Just v <- bondGetInt8
            return $ SeInt8 v
        BT_INT16 -> do
            Just v <- bondGetInt16
            return $ SeInt16 v
        BT_INT32 -> do
            Just v <- bondGetInt32
            return $ SeInt32 v
        BT_INT64 -> do
            Just v <- bondGetInt64
            return $ SeInt64 v
        BT_WSTRING -> do
            Just v <- bondGetWString
            return $ SeWString v
    loop st = do
        (tag, n) <- getFieldHeader
        case tag of
            BT_STOP -> return st
            BT_STOP_BASE -> loop (StreamStruct (Just st) [])
            _ -> do
                e <- getElem tag
                loop $ st { ssElems = ssElems st ++ [(n, e)] }

streamToCompact :: StreamStruct -> BondPut CompactBinaryProto
streamToCompact struct = do
    let BondPut writer = do
        putStreamStruct struct
        BondPut $ putWord8 $ fromIntegral $ fromEnum BT_STOP
    let bs = runPut writer
    putVarInt $ fromIntegral $ Lazy.length bs
    BondPut $ putLazyByteString bs
    where
    putStreamStruct st = do
        when (isJust $ ssBase st) $ do
            putStreamStruct (fromJust $ ssBase st)
            BondPut $ putWord8 $ fromIntegral $ fromEnum BT_STOP_BASE
        mapM_ putField (ssElems st)
    putField (n, e) = do
        putFieldHeader (elemItemType e) n
        putElem e
    putElem (SeBool v) = bondPutBool v
    putElem (SeUInt8 v) = bondPutUInt8 v
    putElem (SeUInt16 v) = bondPutUInt16 v
    putElem (SeUInt32 v) = bondPutUInt32 v
    putElem (SeUInt64 v) = bondPutUInt64 v
    putElem (SeFloat v) = bondPutFloat v
    putElem (SeDouble v) = bondPutDouble v
    putElem (SeString v) = bondPutString v
    putElem (SeStruct v) = streamToCompact v
    putElem (SeList tag xs) = do
        putV2ListHeader tag (length xs)
        mapM_ putElem xs
    putElem (SeSet tag xs) = do
        putV2ListHeader tag (length xs)
        mapM_ putElem xs
    putElem (SeMap (ktag, vtag) xs) = do
        BondPut $ putWord8 $ fromIntegral $ fromEnum ktag
        BondPut $ putWord8 $ fromIntegral $ fromEnum vtag
        putVarInt $ length xs
        forM_ xs $ \(k, v) -> do
            putElem k
            putElem v
    putElem (SeInt8 v) = do
        bondPutInt8 v
    putElem (SeInt16 v) = do
        bondPutInt16 v
    putElem (SeInt32 v) = do
        bondPutInt32 v
    putElem (SeInt64 v) = do
        bondPutInt64 v
    putElem (SeWString v) = do
        bondPutWString v
