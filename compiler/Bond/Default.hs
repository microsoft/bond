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

instance Default Bool where defaultValue = False
instance Default Double where defaultValue = 0
instance Default Float where defaultValue = 0
instance Default Int8 where defaultValue = 0
instance Default Int16 where defaultValue = 0
instance Default Int32 where defaultValue = 0
instance Default Int64 where defaultValue = 0
instance Default Word8 where defaultValue = 0
instance Default Word16 where defaultValue = 0
instance Default Word32 where defaultValue = 0
instance Default Word64 where defaultValue = 0
instance Default (Maybe a) where defaultValue = Nothing
instance Default [a] where defaultValue = []
instance Default ByteString where defaultValue = BS.empty
instance Default Utf8 where defaultValue = Utf8 BS.empty
instance Default Utf16 where defaultValue = Utf16 BS.empty
instance Default (Map a b) where defaultValue = M.empty
instance Default (HashSet a) where defaultValue = H.empty
instance Default (Vector a) where defaultValue = V.empty
instance Default a => Default (Bonded a) where defaultValue = Bonded defaultValue
