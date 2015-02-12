module Bond.Default (
    Default(..)
  ) where

import Bond.Types
import Data.Hashable
import qualified Data.ByteString as BS
import qualified Data.HashSet as H
import qualified Data.Map as M
import qualified Data.Vector as V

class Eq a => Default a where
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
instance Eq a => Default (Maybe a) where defaultValue = Nothing
instance Eq a => Default [a] where defaultValue = []
instance Default Blob where defaultValue = Blob BS.empty
instance Default Utf8 where defaultValue = Utf8 BS.empty
instance Default Utf16 where defaultValue = Utf16 BS.empty
instance (Eq a, Eq b) => Default (Map a b) where defaultValue = M.empty
instance (Eq a, Hashable a) => Default (HashSet a) where defaultValue = H.empty
instance Eq a => Default (Vector a) where defaultValue = V.empty
instance Default a => Default (Bonded a) where defaultValue = Bonded defaultValue
