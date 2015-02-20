{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bond.Types (
    Int,
    Int8,
    Int16,
    Int32,
    Int64,
    Word8,
    Word16,
    Word32,
    Word64,
    Double,
    Float,
    Bool,
    Maybe,
    Utf8(..),
    Utf16(..),
    Blob(..),
    Bonded(..),
    H.HashSet,
    M.Map,
    V.Vector,
    EncodedString(..),
    unpackBonded
  ) where

import Data.Int
import Data.Word
import Data.Hashable
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.HashSet as H
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

newtype Utf8 = Utf8 BS.ByteString
    deriving (Eq, Ord, Hashable)

newtype Utf16 = Utf16 BS.ByteString
    deriving (Eq, Ord, Hashable)

newtype Blob = Blob BS.ByteString
    deriving (Show, Eq, Ord, Hashable)

class EncodedString a where
    fromString :: String -> a

instance EncodedString Utf8 where fromString = Utf8 . T.encodeUtf8 . T.pack
instance EncodedString Utf16 where fromString = Utf16 . T.encodeUtf16LE . T.pack

instance Show Utf8 where show (Utf8 s) = show $ T.unpack $ T.decodeUtf8 s
instance Show Utf16 where show (Utf16 s) = show $ T.unpack $ T.decodeUtf16LE s

type BondedDecoder a = (Lazy.ByteString -> a)
data Bonded a = BondedStream Lazy.ByteString (BondedDecoder a) | BondedObject a

instance Show a => Show (Bonded a) where
    show (BondedStream _ _) = "BondedStream"
    show (BondedObject v) = show v

instance Eq a => Eq (Bonded a) where
    (BondedObject v1) == (BondedObject v2) = v1 == v2
    (BondedStream s1 _) == (BondedStream s2 _) = s1 == s2

unpackBonded :: Bonded a -> a
unpackBonded (BondedObject v) = v
unpackBonded (BondedStream s decoder) = decoder s
