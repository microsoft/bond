module Bond.Internal (
    Int8,
    Int16,
    Int32,
    Int64,
    Word8,
    Word16,
    Word32,
    Word64,
    Map,
    ByteString,
    HashSet,
    Vector,
    Bonded
  ) where

import Data.Int
import Data.Word
import Data.Map
import Data.ByteString
import Data.HashSet
import Data.Vector

data Bonded a = Bonded a
    deriving Show
