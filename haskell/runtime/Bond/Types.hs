{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bond.Types (
    Blob(..),
    Bonded(..),
    Bool,
    Double,
    EncodedString(..),
    Float,
    H.HashSet,
    Int,
    Int16,
    Int32,
    Int64,
    Int8,
    Maybe,
    M.Map,
    Utf16(..),
    Utf8(..),
    V.Vector,
    Word16,
    Word32,
    Word64,
    Word8,
    ProtoSig(..),
    compactSig,
    compactV1Sig,
    fastSig,
    simpleSig,
    simpleV1Sig
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

newtype ProtoSig = ProtoSig Word32
    deriving Eq

compactSig, compactV1Sig, simpleSig, simpleV1Sig, fastSig :: ProtoSig
compactSig = ProtoSig 0x43420200
compactV1Sig = ProtoSig 0x43420100
simpleSig = ProtoSig 0x53500200
simpleV1Sig = ProtoSig 0x53500100
fastSig = ProtoSig 0x4D460100

data Bonded a = BondedStream ProtoSig Lazy.ByteString | BondedObject a

instance Show a => Show (Bonded a) where
    show BondedStream{} = "BondedStream"
    show (BondedObject v) = show v

instance Eq a => Eq (Bonded a) where
    (BondedObject v1) == (BondedObject v2) = v1 == v2
    _ == _ = True
--    (BondedStream s1 p1 v1) == (BondedStream s2 p1 v1) = unpackBonded p1 v1 s1 == unpackBonded p2 v2 s2
