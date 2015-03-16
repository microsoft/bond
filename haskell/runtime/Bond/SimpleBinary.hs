{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls, GADTs, MultiWayIf, InstanceSigs #-}
module Bond.SimpleBinary (
    deserializeSimpleV1,
    serializeSimpleV1,
    deserializeSimple,
    serializeSimple,
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
import Data.Bits
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
    bondGetMap = getMapV1
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

getMapV1 :: forall a b. (Ord a, BondBinary a, BondBinary b) => BondGet SimpleBinaryV1Proto (Maybe (M.Map a b))
getMapV1 = do
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

instance BondBinaryProto SimpleBinaryProto where
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
        putVarInt $ BS.length s
        BondPut $ putByteString s
    bondPutWString (Utf16 s) = do
        putVarInt $ BS.length s `div` 2
        BondPut $ putByteString s
    bondPutList xs = do
        putVarInt $ length xs
        mapM_ bondPut xs
    bondPutMaybe Nothing = fail "can't put defaultNothing value to simple stream"
    bondPutMaybe (Just v) = bondPut v
    bondPutNullable = bondPutList . maybeToList
    bondPutSet = bondPutList . H.toList
    bondPutMap m = do
        putVarInt $ M.size m
        forM_ (M.toList m) $ \(k, v) -> do
            bondPut k
            bondPut v
    bondPutVector xs = do
        putVarInt $ V.length xs
        V.mapM_ bondPut xs
    bondPutBlob (Blob b) = do
        putVarInt $ BS.length b
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
        n <- getVarInt
        Just . Utf8 <$> BondGet (getByteString n)
    bondGetWString = do
        n <- getVarInt
        Just . Utf16 <$> BondGet (getByteString $ n * 2)
    bondGetBlob = do
        n <- getVarInt
        Just . Blob <$> BondGet (getByteString n)
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
        size <- BondGet getWord32le
        sig <- BondGet getWord32be
        bs <- BondGet $ getLazyByteString (fromIntegral $ size - 4)
        return $ Just $ BondedStream (ProtoSig sig) bs
    bondGetStruct = Just <$> readStruct (bondGetInfo Proxy)

getMap :: forall a b. (Ord a, BondBinary a, BondBinary b) => BondGet SimpleBinaryProto (Maybe (M.Map a b))
getMap = do
    n <- getVarInt
    elems <- replicateM n $ do
        k <- bondGet
        v <- bondGet
        return $ seqPair (k, v)
    return $ M.fromList <$> sequence elems
    where
    seqPair (Just a, Just b) = Just (a, b)
    seqPair _ = Nothing

getList :: forall a. BondBinary a => BondGet SimpleBinaryProto (Maybe [a])
getList = do
    n <- getVarInt
    elems <- replicateM n bondGet
    return $ sequence elems

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

deserializeSimple :: forall a. BondStruct a => Lazy.ByteString -> Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, a) 
deserializeSimple = let BondGet g = bondGet :: BondGet SimpleBinaryProto (Maybe a)
                      in runGetOrFail (fromJust <$> g)

serializeSimple :: BondStruct a => a -> Lazy.ByteString
serializeSimple v = let BondPut g = bondPut v :: BondPut SimpleBinaryProto
                      in runPut g
