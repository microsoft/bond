module Bond.Schema where

import Bond.Wire

data FieldInfo = FieldInfo { fiName :: String, fiType :: ItemType, fiOrdinal :: Ordinal }
newtype StructSchema = StructSchema [FieldInfo]
