module Bond.Stream where

import Bond.Types
import Bond.Wire

data StreamStruct = StreamStruct { ssBase :: Maybe StreamStruct, ssElems :: [(Ordinal, StreamElement)] }
    deriving Show

data StreamElement =
      SeBool Bool
    | SeUInt8 Word8
    | SeUInt16 Word16
    | SeUInt32 Word32
    | SeUInt64 Word64
    | SeFloat Float
    | SeDouble Double
    | SeString Utf8
    | SeStruct StreamStruct
    | SeList ItemType [StreamElement]
    | SeSet ItemType [StreamElement]
    | SeMap (ItemType, ItemType) [(StreamElement, StreamElement)]
    | SeInt8 Int8
    | SeInt16 Int16
    | SeInt32 Int32
    | SeInt64 Int64
    | SeWString Utf16
    deriving Show

elemItemType :: StreamElement -> ItemType
elemItemType (SeBool _) = BT_BOOL
elemItemType (SeUInt8 _) = BT_UINT8
elemItemType (SeUInt16 _) = BT_UINT16
elemItemType (SeUInt32 _) = BT_UINT32
elemItemType (SeUInt64 _) = BT_UINT64
elemItemType (SeFloat _) = BT_FLOAT
elemItemType (SeDouble _) = BT_DOUBLE
elemItemType (SeString _) = BT_STRING
elemItemType (SeStruct _) = BT_STRUCT
elemItemType (SeList _ _) = BT_LIST
elemItemType (SeSet _ _) = BT_SET
elemItemType (SeMap _ _) = BT_MAP
elemItemType (SeInt8 _) = BT_INT8
elemItemType (SeInt16 _) = BT_INT16
elemItemType (SeInt32 _) = BT_INT32
elemItemType (SeInt64 _) = BT_INT64
elemItemType (SeWString _) = BT_WSTRING
