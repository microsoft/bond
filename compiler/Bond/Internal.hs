module Bond.Internal (
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
    Bonded,
    Utf8,
    Utf16,
    BS.ByteString,
    H.HashSet,
    M.Map,
    V.Vector,
    Default(..),
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
instance Default Utf8 where defaultValue = Utf8 BS.empty
instance Default Utf16 where defaultValue = Utf16 BS.empty
instance Default (M.Map a b) where defaultValue = M.empty
instance Default (H.HashSet a) where defaultValue = H.empty
instance Default (V.Vector a) where defaultValue = V.empty
instance Default a => Default (Bonded a) where defaultValue = Bonded defaultValue

class EncodedString a where
    fromString :: String -> a

instance EncodedString Utf8 where fromString = Utf8 . T.encodeUtf8 . T.pack
instance EncodedString Utf16 where fromString = Utf16 . T.encodeUtf16LE . T.pack
