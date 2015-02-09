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
    Bonded(..),
    Utf8(..),
    Utf16(..),
    Blob(..),
    H.HashSet,
    M.Map,
    V.Vector,
    EncodedString(..)
  ) where

import Data.Int
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.HashSet as H
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

newtype Bonded a = Bonded a
    deriving Show

newtype Utf8 = Utf8 BS.ByteString
    deriving Show

newtype Utf16 = Utf16 BS.ByteString
    deriving Show

newtype Blob = Blob BS.ByteString
    deriving Show

class EncodedString a where
    fromString :: String -> a

instance EncodedString Utf8 where fromString = Utf8 . T.encodeUtf8 . T.pack
instance EncodedString Utf16 where fromString = Utf16 . T.encodeUtf16LE . T.pack
