module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Data.Int

import Unittest.Compat.Compat
import qualified Data.ByteString.Lazy as BS

import Bond.API
import Bond.Imports (encodeInt, decodeInt, EncodedInt(..))

type GetFunc t a = BondGet t a -> BS.ByteString -> Either (BS.ByteString, Int64, String) (BS.ByteString, Int64, a)
type PutFunc t = BondPut t -> BS.ByteString

fastBinaryData, compactBinaryV2Data :: String
fastBinaryData = "/home/blaze/bond/test/compat/data/compat.fast.dat"
compactBinaryV1Data = "/home/blaze/bond/test/compat/data/compat.compact.dat"
compactBinaryV2Data = "/home/blaze/bond/test/compat/data/compat.compact2.dat"

main :: IO ()
main = defaultMain [
    testGroup "Fast Binary protocol" [
        testCase "parsing existing data" $ testParseCompat runFastBinaryGet fastBinaryData,
        testCase "saving and parsing data" $ testParseOwnOutput runFastBinaryGet runFastBinaryPut fastBinaryData
    ],
    testGroup "Compact Binary protocol" [
        testGroup "Version 1" [
            testCase "parsing existing data" $ testParseCompat runCompactBinaryV1Get compactBinaryV1Data,
            testCase "saving and parsing data" $ testParseOwnOutput runCompactBinaryV1Get runCompactBinaryV1Put compactBinaryV1Data
        ],
        testGroup "Version 2" [
            testCase "parsing existing data" $ testParseCompat runCompactBinaryV2Get compactBinaryV2Data,
            testCase "saving and parsing data" $ testParseOwnOutput runCompactBinaryV2Get runCompactBinaryV2Put compactBinaryV2Data
        ],
        testProperty "check int conversion in CompactBinary" compactDecodeEncodeInt
    ],
    testCase "Check for identical read results" testAllReadSameData
 ]

testParseCompat :: BondBinaryProto t => GetFunc t Compat -> String -> Assertion
testParseCompat get file = do
    b <- BS.readFile file
    let parse = get bondGet b
    uncurry assertBool $ case parse of
                            Right (rest, _, _) | BS.null rest -> (undefined, True)
                            Right (_, _, _) -> ("Not all input consumed", False)
                            Left (_, _, msg) -> (msg, False)

testParseOwnOutput :: BondBinaryProto t => GetFunc t Compat -> PutFunc t -> String -> Assertion
testParseOwnOutput get put file = do
    b <- BS.readFile file
    let s = runGetOrFail b
    let b' = put (bondPut s)
    let s' = runGetOrFail b'
    assertEqual "Saved value do not match parsed one" s s'
    where
    runGetOrFail s = case get bondGet s of
                        Right (rest, _, msg) | BS.null rest -> msg
                        Right (_, _, _) -> error "Not all input consumed"
                        Left (_, _, msg) -> error msg

testAllReadSameData :: Assertion
testAllReadSameData = do
    fastbin <- BS.readFile fastBinaryData
    let fastrec = (runGetOrFail runFastBinaryGet fastbin) :: Compat
    cv1bin <- BS.readFile compactBinaryV1Data
    let cv1rec = runGetOrFail runCompactBinaryV1Get cv1bin
    cv2bin <- BS.readFile compactBinaryV2Data
    let cv2rec = runGetOrFail runCompactBinaryV2Get cv2bin
    assertEqual "FastBinary read do not match CompactBinary v1 read" fastrec cv1rec
    assertEqual "FastBinary read do not match CompactBinary v2 read" fastrec cv2rec
    where
    runGetOrFail get s = case get bondGet s of
                            Right (rest, _, msg) | BS.null rest -> msg
                            Right (_, _, _) -> error "Not all input consumed"
                            Left (_, _, msg) -> error msg

compactDecodeEncodeInt :: Int64 -> Bool
compactDecodeEncodeInt x = EncodedInt x == decodeInt (encodeInt $ EncodedInt x)
