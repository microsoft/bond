module Bond.Default (
    Default(..)
  ) where

import Bond.Types
import qualified Data.ByteString as BS
import qualified Data.HashSet as H
import qualified Data.Map as M
import qualified Data.Vector as V

class Default a where
    defaultValue :: a
    -- optimized type-aware comparison with default value
    equalToDefault :: a -> a -> Bool

instance Default Bool where
    defaultValue = False
    equalToDefault = (==)
instance Default Double where
    defaultValue = 0
    equalToDefault = (==)
instance Default Float where
    defaultValue = 0
    equalToDefault = (==)
instance Default Int8 where
    defaultValue = 0
    equalToDefault = (==)
instance Default Int16 where
    defaultValue = 0
    equalToDefault = (==)
instance Default Int32 where
    defaultValue = 0
    equalToDefault = (==)
instance Default Int64 where
    defaultValue = 0
    equalToDefault = (==)
instance Default Word8 where
    defaultValue = 0
    equalToDefault = (==)
instance Default Word16 where
    defaultValue = 0
    equalToDefault = (==)
instance Default Word32 where
    defaultValue = 0
    equalToDefault = (==)
instance Default Word64 where
    defaultValue = 0
    equalToDefault = (==)
instance Default (Maybe a) where
    defaultValue = Nothing
    -- default value for nullable is always null (CPP codegen even crashes when default is set)
    equalToDefault Nothing Nothing = True
    equalToDefault _ _ = False
instance Default [a] where
    defaultValue = []
    -- default value for list is always []
    equalToDefault a b = null a && null b
instance Default Blob where
    defaultValue = Blob BS.empty
    -- default value for blob is always BS.empty
    equalToDefault (Blob a) (Blob b) = BS.null a && BS.null b
instance Default Utf8 where
    defaultValue = Utf8 BS.empty
    equalToDefault = (==)
instance Default Utf16 where
    defaultValue = Utf16 BS.empty
    equalToDefault = (==)
instance Default (Map a b) where
    defaultValue = M.empty
    -- default value for map is always M.empty
    equalToDefault a b = M.null a && M.null b
instance Default (HashSet a) where
    defaultValue = H.empty
    -- default value for set is always H.empty
    equalToDefault a b = H.null a && H.null b
instance Default (Vector a) where
    defaultValue = V.empty
    -- default value for vector is always V.empty
    equalToDefault a b = V.null a && V.null b
instance Default a => Default (Bonded a) where
    defaultValue = BondedObject defaultValue
    -- Default value check is performed to decide if field needs to be written.
    -- Bonded streams must always be written.
    equalToDefault _ _ = False
