module Bond.Internal (
    Int8,
    Int16,
    Int32,
    Int64,
    Word8,
    Word16,
    Word32,
    Word64,
    M.Map,
    BS.ByteString,
    H.HashSet,
    V.Vector,
    Bonded,
    Default(..)
  ) where

import Data.Int
import Data.Word
import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.HashSet as H
import qualified Data.Vector as V

newtype Bonded a = Bonded a
    deriving Show

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
instance Default BS.ByteString where defaultValue = BS.empty
instance Default (M.Map a b) where defaultValue = M.empty
instance Default (H.HashSet a) where defaultValue = H.empty
instance Default (V.Vector a) where defaultValue = V.empty
instance Default a => Default (Bonded a) where defaultValue = Bonded defaultValue
