{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls, GADTs, MultiWayIf, InstanceSigs #-}
module Bond.SimpleBinary (
    deserializeSimpleV1,
    serializeSimpleV1,
--    deserializeSimple,
--    serializeSimple,
    SimpleBinaryV1Proto,
    SimpleBinaryProto
  ) where

import Bond.BinaryProto
import Bond.Cast
import Bond.Default
import Bond.Schema
import Bond.Types
import Control.Applicative
import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
--import Data.Bits
import Data.Maybe
import Data.Proxy
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.HashSet as H
import qualified Data.Map as M
import qualified Data.Vector as V

data SimpleBinaryV1Proto
data SimpleBinaryProto

instance BondBinaryProto SimpleBinaryV1Proto where
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
        BondPut $ putWord32le $ fromIntegral $ BS.length s
        BondPut $ putByteString s
    bondPutWString (Utf16 s) = do
        BondPut $ putWord32le $ fromIntegral $ BS.length s `div` 2
        BondPut $ putByteString s
    bondPutList xs = do
        BondPut $ putWord32le $ fromIntegral $ length xs
        mapM_ bondPut xs
    bondPutMaybe Nothing = fail "can't put defaultNothing value to simple stream"
    bondPutMaybe (Just v) = bondPut v
    bondPutNullable = bondPutList . maybeToList
    bondPutSet = bondPutList . H.toList
    bondPutMap m = do
        BondPut $ putWord32le $ fromIntegral $ M.size m
        forM_ (M.toList m) $ \(k, v) -> do
            bondPut k
            bondPut v
    bondPutVector xs = do
        BondPut $ putWord32le $ fromIntegral $ V.length xs
        V.mapM_ bondPut xs
    bondPutBlob (Blob b) = do
        BondPut $ putWord32le $ fromIntegral $ BS.length b
        BondPut $ putByteString b
    bondPutBonded (BondedObject a) = bondPut a
    bondPutBonded (BondedStream (ProtoSig sig) s) = do
        BondPut $ putWord32le $ fromIntegral (4 + Lazy.length s)
        BondPut $ putWord32be sig
        BondPut $ putLazyByteString s
    bondPutStruct = saveStruct (bondGetInfo Proxy)

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
        n <- BondGet getWord32le
        Just . Utf8 <$> BondGet (getByteString $ fromIntegral n)
    bondGetWString = do
        n <- BondGet getWord32le
        Just . Utf16 <$> BondGet (getByteString $ fromIntegral $ n * 2)
    bondGetBlob = do
        n <- BondGet getWord32le
        Just . Blob <$> BondGet (getByteString $ fromIntegral n)
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
    bondGetBonded :: forall a. BondBinary a => BondGet SimpleBinaryV1Proto (Maybe (Bonded a))
    bondGetBonded = do
        size <- BondGet getWord32le
        sig <- BondGet getWord32be
        bs <- BondGet $ getLazyByteString (fromIntegral $ size - 4)
        return $ Just $ BondedStream (ProtoSig sig) bs
    bondGetStruct = Just <$> readStruct (bondGetInfo Proxy)

getMap :: forall a b t. (BondBinaryProto t, Ord a, BondBinary a, BondBinary b) => BondGet t (Maybe (M.Map a b))
getMap = do
    n <- fromIntegral <$> BondGet getWord32le
    elems <- replicateM n $ do
        k <- bondGet
        v <- bondGet
        return $ seqPair (k, v)
    return $ M.fromList <$> sequence elems
    where
    seqPair (Just a, Just b) = Just (a, b)
    seqPair _ = Nothing

getListV1 :: forall a. BondBinary a => BondGet SimpleBinaryV1Proto (Maybe [a])
getListV1 = do
    n <- fromIntegral <$> BondGet getWord32le
    elems <- replicateM n bondGet
    return $ sequence elems

readStruct :: forall a b t. BondBinaryProto t => StructInfo a b -> BondGet t a
readStruct (StructInfo pa pb) = do
    def <- case pb of
        Nothing -> return defaultValue
        Just p -> do
            base <- readStruct (bondGetInfo p)
            return $ bondSetBase defaultValue base
    update <- foldM setField id schema
    return $ update def
    where
    StructSchema schema = bondGetSchema pa
    setField f fi = do
        fieldMod <- bondSetField (fiOrdinal fi) (fiType fi)
        case fieldMod of
            Just func -> return (func . f)
            Nothing -> fail "error reading simple protocol field"

saveStruct :: BondBinaryProto t => StructInfo a b -> a -> BondPut t
saveStruct (StructInfo pa pb) a = do
    when (isJust pb) $ saveStruct (bondGetInfo $ fromJust pb) (bondGetBase a)
    putFields (bondGetSchema pa)
    where
    putFields (StructSchema fields) = forM_ fields $ \(FieldInfo _ _ n) -> bondPutField a n

deserializeSimpleV1 :: forall a. BondStruct a => Lazy.ByteString -> Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, a) 
deserializeSimpleV1 = let BondGet g = bondGet :: BondGet SimpleBinaryV1Proto (Maybe a)
                        in runGetOrFail (fromJust <$> g)

serializeSimpleV1 :: BondStruct a => a -> Lazy.ByteString
serializeSimpleV1 v = let BondPut g = bondPut v :: BondPut SimpleBinaryV1Proto
                        in runPut g

{-
instance BondBinaryProto SimpleBinaryProto where
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
    bondPutMap m = do
        BondPut $ putWord8 $ typeIdOf (head $ M.keys m)
        BondPut $ putWord8 $ typeIdOf (head $ M.elems m)
        putVarInt $ M.size m
        forM_ (M.toList m) $ \(k, v) -> do
            bondPut k
            bondPut v
    bondPutVector xs = do
        putV2ListHeader (wireTypeOf $ V.head xs) (V.length xs)
        V.mapM_ bondPut xs
    bondPutBlob (Blob b) = do
        putV2ListHeader BT_INT8 (BS.length b)
        BondPut $ putByteString b
    bondPutBonded (BondedObject a) = bondPut a
    bondPutBonded (BondedStream sig s) | sig == compactSig = BondPut $ putLazyByteString s
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
    bondGetBonded :: forall a. BondBinary a => BondGet SimpleBinaryProto (Maybe (Bonded a))
    bondGetBonded = do
        let try (BondGet g) = BondGet $ lookAhead g
        size <- try $ do
            start <- BondGet bytesRead
            skipValueV2 (getWireType (Proxy :: Proxy a))
            end <- BondGet bytesRead
            return (end - start)
        Just . BondedStream compactSig <$> BondGet (getLazyByteString size)
    bondGetStruct = Just <$> getStruct (bondGetInfo Proxy)

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


getV2ListHeader :: BondGet SimpleBinaryProto (ItemType, Int)
getV2ListHeader = do
    v <- BondGet getWord8
    if v `shiftR` 5 /= 0
        then return (wireType (v .&. 0x1f), fromIntegral (v `shiftR` 5) - 1)
        else do
                num <- getVarInt
                return (wireType v, num)

putV2ListHeader :: ItemType -> Int -> BondPut SimpleBinaryProto
putV2ListHeader t n = do
    let tag = fromIntegral $ fromEnum t
    if n < 7
        then BondPut $ putWord8 $ tag .|. fromIntegral ((1 + n) `shiftL` 5)
        else do
            BondPut $ putWord8 tag
            putVarInt $ n

skipValueV2 :: ItemType -> BondGet SimpleBinaryProto ()
skipValueV2 BT_LIST = do
    (tag, n) <- getV2ListHeader
    replicateM_ n (skipValue tag)
skipValueV2 BT_STRUCT = do
    len <- getVarInt
    BondGet $ skip len
skipValueV2 t = skipValueCommon t

getList :: forall a. BondBinary a => BondGet SimpleBinaryProto (Maybe [a])
getList = do
    (tag, n) <- getV2ListHeader
    if checkWireType (Proxy :: Proxy a) tag
        then do
            elems <- replicateM n bondGet
            return $ sequence elems
        else do
            replicateM_ n (skipValue tag)
            return Nothing

putStruct :: StructInfo a b -> a -> BondPut SimpleBinaryProto
putStruct p v = do
    let BondPut writer = saveStruct BT_STOP p v :: BondPut SimpleBinaryProto
    let bs = runPut writer
    putVarInt $ fromIntegral $ Lazy.length bs
    BondPut $ putLazyByteString bs

getStruct :: forall a b. StructInfo a b -> BondGet SimpleBinaryProto a
getStruct p = do
    void getVarInt -- FIXME use "isolate" from Data.Binary >= 0.7.2.0
    readStruct p

deserializeSimple :: forall a. BondStruct a => Lazy.ByteString -> Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, a) 
deserializeSimple = let BondGet g = bondGet :: BondGet SimpleBinaryProto (Maybe a)
                      in runGetOrFail (fromJust <$> g)

serializeSimple :: BondStruct a => a -> Lazy.ByteString
serializeSimple v = let BondPut g = bondPut v :: BondPut SimpleBinaryProto
                      in runPut g
-}
