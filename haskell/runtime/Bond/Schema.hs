module Bond.Schema where

import Bond.Wire

data FieldInfo = FieldInfo { fiName :: String, fiOrdinal :: Ordinal }
newtype StructSchema t = StructSchema [FieldInfo]
