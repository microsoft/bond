{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving, TypeFamilies, GADTs #-}
module Bond.BinaryProto (
        BondBinary(..),
        BondStruct(..),
        BondBinaryProto(..),
        BondGet(..),
        BondPutM(..),
        BondPut,
        StructInfo(..),
        VoidBase,
--        fromWireType,
--        toWireType
--    skipBinaryValue
  ) where

import Bond.Default
import Bond.Types
import Bond.Wire
import Control.Applicative
import Data.Binary.Get
import Data.Binary.Put
import Data.Hashable
import Bond.Schema
import Data.Proxy
import qualified Data.HashSet as H
import qualified Data.Map as M
import qualified Data.Vector as V

class WireType a => BondBinary a where
    -- | Read field value.
    -- Returns Just a if ok, Nothing if schema mismatch, fails on irrecoverable error
    bondGet :: BondBinaryProto t => BondGet t (Maybe a)
    -- | Put field into stream.
    bondPut :: BondBinaryProto t => a -> BondPut t

data StructInfo a b where
    StructInfo :: (BondStruct a, BondStruct b, Base a ~ b) => Proxy a -> Maybe (Proxy b) -> StructInfo a b

class (Default a, BondBinary a) => BondStruct a where
    type Base a :: *
    bondGetSchema :: Proxy a -> StructSchema
    bondGetInfo :: Proxy a -> StructInfo a (Base a)
    bondSetField :: BondBinaryProto t => Ordinal -> ItemType -> BondGet t (Maybe (a -> a))
    bondSetBase :: a -> Base a -> a
    bondGetBase :: a -> Base a
    bondFieldHasDefaultValue :: a -> Ordinal -> Bool
    bondPutField :: BondBinaryProto t => a -> Ordinal -> BondPut t

data VoidBase
instance Default VoidBase where
    defaultValue = error "call to VoidBase"
    equalToDefault = error "call to VoidBase"
instance WireType VoidBase where
    getWireType = error "call to VoidBase"
instance BondBinary VoidBase where
    bondGet = error "call to VoidBase"
    bondPut = error "call to VoidBase"
instance BondStruct VoidBase where
    type Base VoidBase = VoidBase
    bondGetSchema = error "call to VoidBase"
    bondGetInfo = error "call to VoidBase"
    bondSetField = error "call to VoidBase"
    bondSetBase = error "call to VoidBase"
    bondGetBase = error "call to VoidBase"
    bondFieldHasDefaultValue = error "call to VoidBase"
    bondPutField = error "call to VoidBase"

newtype BondGet t a = BondGet (Get a)
    deriving (Functor, Applicative, Monad)
newtype BondPutM t a = BondPut (PutM a)
    deriving (Functor, Applicative, Monad)
type BondPut t = BondPutM t ()

class BondBinaryProto t where
    bondPutBool :: Bool -> BondPut t
    bondPutUInt8 :: Word8 -> BondPut t
    bondPutUInt16 :: Word16 -> BondPut t
    bondPutUInt32 :: Word32 -> BondPut t
    bondPutUInt64 :: Word64 -> BondPut t
    bondPutInt8 :: Int8 -> BondPut t
    bondPutInt16 :: Int16 -> BondPut t
    bondPutInt32 :: Int32 -> BondPut t
    bondPutInt64 :: Int64 -> BondPut t
    bondPutFloat :: Float -> BondPut t
    bondPutDouble :: Double -> BondPut t
    bondPutString :: Utf8 -> BondPut t
    bondPutWString :: Utf16 -> BondPut t
    bondPutBlob :: Blob -> BondPut t
    bondPutList :: BondBinary a => [a] -> BondPut t
    bondPutVector :: BondBinary a => V.Vector a -> BondPut t
    bondPutSet :: BondBinary a => H.HashSet a -> BondPut t
    bondPutMap :: (BondBinary k, BondBinary v) => M.Map k v -> BondPut t
    bondPutNullable :: BondBinary a => Maybe a -> BondPut t
    bondPutMaybe :: BondBinary a => Maybe a -> BondPut t
    bondPutBonded :: BondBinary a => Bonded a -> BondPut t
    bondPutStruct :: BondStruct a => a -> BondPut t

    bondGetBool :: BondGet t (Maybe Bool)
    bondGetUInt8 :: BondGet t (Maybe Word8)
    bondGetUInt16 :: BondGet t (Maybe Word16)
    bondGetUInt32 :: BondGet t (Maybe Word32)
    bondGetUInt64 :: BondGet t (Maybe Word64)
    bondGetInt8 :: BondGet t (Maybe Int8)
    bondGetInt16 :: BondGet t (Maybe Int16)
    bondGetInt32 :: BondGet t (Maybe Int32)
    bondGetInt64 :: BondGet t (Maybe Int64)
    bondGetFloat :: BondGet t (Maybe Float)
    bondGetDouble :: BondGet t (Maybe Double)
    bondGetString :: BondGet t (Maybe Utf8)
    bondGetWString :: BondGet t (Maybe Utf16)
    bondGetBlob :: BondGet t (Maybe Blob)
    bondGetList :: BondBinary a => BondGet t (Maybe [a])
    bondGetVector :: BondBinary a => BondGet t (Maybe (V.Vector a))
    bondGetSet :: (Eq a, Hashable a, BondBinary a) => BondGet t (Maybe (H.HashSet a))
    bondGetMap :: (Ord k, BondBinary k, BondBinary v) => BondGet t (Maybe (M.Map k v))
    bondGetNullable :: BondBinary a => BondGet t (Maybe (Maybe a))
    bondGetBonded :: BondBinary a => BondGet t (Maybe (Bonded a))
    bondGetStruct :: BondStruct a => BondGet t (Maybe a)

instance BondBinary Float where
    bondGet = bondGetFloat
    bondPut = bondPutFloat

instance BondBinary Double where
    bondGet = bondGetDouble
    bondPut = bondPutDouble

instance BondBinary Bool where
    bondGet = bondGetBool
    bondPut = bondPutBool

instance BondBinary Int8 where
    bondGet = bondGetInt8
    bondPut = bondPutInt8

instance BondBinary Int16 where
    bondGet = bondGetInt16
    bondPut = bondPutInt16

instance BondBinary Int32 where
    bondGet = bondGetInt32
    bondPut = bondPutInt32

instance BondBinary Int64 where
    bondGet = bondGetInt64
    bondPut = bondPutInt64

instance BondBinary Word8 where
    bondGet = bondGetUInt8
    bondPut = bondPutUInt8

instance BondBinary Word16 where
    bondGet = bondGetUInt16
    bondPut = bondPutUInt16

instance BondBinary Word32 where
    bondGet = bondGetUInt32
    bondPut = bondPutUInt32

instance BondBinary Word64 where
    bondGet = bondGetUInt64
    bondPut = bondPutUInt64

instance BondBinary Utf8 where
    bondGet = bondGetString
    bondPut = bondPutString

instance BondBinary Utf16 where
    bondGet = bondGetWString
    bondPut = bondPutWString

instance BondBinary Blob where
    bondGet = bondGetBlob
    bondPut = bondPutBlob

instance BondBinary a => BondBinary [a] where
    bondGet = bondGetList
    bondPut = bondPutList

instance BondBinary a => BondBinary (V.Vector a) where
    bondGet = bondGetVector
    bondPut = bondPutVector

instance (Eq a, Hashable a, BondBinary a) => BondBinary (H.HashSet a) where
    bondGet = bondGetSet
    bondPut = bondPutSet

instance (Ord k, BondBinary k, BondBinary v) => BondBinary (M.Map k v) where
    bondGet = bondGetMap
    bondPut = bondPutMap

instance BondBinary a => BondBinary (Maybe a) where
    bondGet = bondGetNullable
    bondPut = bondPutNullable

instance BondBinary a => BondBinary (Bonded a) where
    bondGet = bondGetBonded
    bondPut = bondPutBonded
