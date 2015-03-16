{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls, GADTs, MultiWayIf, InstanceSigs #-}
module Bond.FastBinary (
    deserializeFast,
    serializeFast,
    FastBinaryProto
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

checkWireType :: WireType a => Proxy a -> ItemType -> Bool
checkWireType p t = getWireType p == t

data FastBinaryProto

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

instance BondBinaryProto FastBinaryProto where
    bondPutBool True = BondPut $ putWord8 1
    bondPutBool False = BondPut $ putWord8 0
    bondPutUInt8 = BondPut . putWord8
    bondPutUInt16 = BondPut . putWord16le
    bondPutUInt32 = BondPut . putWord32le
    bondPutUInt64 = BondPut . putWord64le
    bondPutInt8 = BondPut . putWord8 . fromIntegral
    bondPutInt16 = BondPut . putWord16le . fromIntegral
    bondPutInt32 = BondPut . putWord32le . fromIntegral
    bondPutInt64 = BondPut . putWord64le . fromIntegral
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
    bondPutBonded (BondedStream sig s) | sig == fastSig = BondPut $ putLazyByteString s
    bondPutStruct = saveStruct BT_STOP (bondGetInfo Proxy)

    bondGetBool = do
        v <- BondGet getWord8
        return $ Just $ v /= 0
    bondGetUInt8 = Just <$> BondGet getWord8
    bondGetUInt16 = Just <$> BondGet getWord16le
    bondGetUInt32 = Just <$> BondGet getWord32le
    bondGetUInt64 = Just <$> BondGet getWord64le
    bondGetInt8 = Just . fromIntegral <$> BondGet getWord8
    bondGetInt16 = Just . fromIntegral <$> BondGet getWord16le
    bondGetInt32 = Just . fromIntegral <$> BondGet getWord32le
    bondGetInt64 = Just . fromIntegral <$> BondGet getWord64le
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
    bondGetBonded :: forall a. BondBinary a => BondGet FastBinaryProto (Maybe (Bonded a))
    bondGetBonded = do
        let try (BondGet g) = BondGet $ lookAhead g
        size <- try $ do
            start <- BondGet bytesRead
            skipValue (getWireType (Proxy :: Proxy a))
            end <- BondGet bytesRead
            return (end - start)
        Just . BondedStream fastSig <$> BondGet (getLazyByteString size)
    bondGetStruct = Just <$> readStruct (bondGetInfo Proxy)

getMap :: forall a b t. (BondBinaryProto t, Ord a, BondBinary a, BondBinary b) => BondGet t (Maybe (M.Map a b))
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

getList :: forall a. BondBinary a => BondGet FastBinaryProto (Maybe [a])
getList = do
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
    tag <- wireType <$> BondGet getWord8
    n <- if tag == BT_STOP || tag == BT_STOP_BASE then return 0 else BondGet getWord16le
    return (tag, Ordinal n)

readStruct :: forall a b t. BondBinaryProto t => StructInfo a b -> BondGet t a
readStruct (StructInfo _ pb) = do
    def <- case pb of
        Nothing -> return defaultValue
        Just p -> do
            base <- readStruct (bondGetInfo p)
            return $ bondSetBase defaultValue base
    getFields def

getFields :: forall a t. (BondStruct a, BondBinaryProto t) => a -> BondGet t a
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
    putFieldHeader t (Ordinal n) = do
        BondPut $ putWord8 $ fromIntegral $ fromEnum t
        BondPut $ putWord16le n

skipValue :: ItemType -> BondGet t ()
skipValue BT_STOP = fail "internal error: skipValue BT_STOP"
skipValue BT_STOP_BASE = fail "internal error: skipValue BT_STOP_BASE"
skipValue BT_BOOL = BondGet $ skip 1
skipValue BT_UINT8 = BondGet $ skip 1
skipValue BT_UINT16 = BondGet $ skip 2
skipValue BT_UINT32 = BondGet $ skip 4
skipValue BT_UINT64 = BondGet $ skip 8
skipValue BT_FLOAT = BondGet $ skip 4
skipValue BT_DOUBLE = BondGet $ skip 8
skipValue BT_INT8 = BondGet $ skip 1
skipValue BT_INT16 = BondGet $ skip 2
skipValue BT_INT32 = BondGet $ skip 4
skipValue BT_INT64 = BondGet $ skip 8
skipValue BT_STRING = do
    n <- getVarInt
    BondGet $ skip n
skipValue BT_WSTRING = do
    n <- getVarInt
    BondGet $ skip (n * 2)
skipValue BT_LIST = do
    t <- wireType <$> BondGet getWord8
    n <- getVarInt
    replicateM_ n (skipValue t)
skipValue BT_SET = skipValue BT_LIST
skipValue BT_MAP = do
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

deserializeFast :: forall a. BondStruct a => Lazy.ByteString -> Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, a) 
deserializeFast = let BondGet g = bondGet :: BondGet FastBinaryProto (Maybe a)
                        in runGetOrFail (fromJust <$> g)

serializeFast :: BondStruct a => a -> Lazy.ByteString
serializeFast v = let BondPut g = bondPut v :: BondPut FastBinaryProto
                        in runPut g
