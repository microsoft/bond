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

fastBinaryData, compactBinaryV1Data, compactBinaryV2Data, simpleBinaryV1Data, simpleBinaryV2Data :: String
fastBinaryData = "/home/blaze/bond/test/compat/data/compat.fast.dat"
compactBinaryV1Data = "/home/blaze/bond/test/compat/data/compat.compact.dat"
compactBinaryV2Data = "/home/blaze/bond/test/compat/data/compat.compact2.dat"
simpleBinaryV1Data = "/home/blaze/bond/test/compat/data/compat.simple.dat"
simpleBinaryV2Data = "/home/blaze/bond/test/compat/data/compat.simple2.dat"

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
            testCase "parsing existing data" $ testParseCompat runCompactBinaryGet compactBinaryV2Data,
            testCase "saving and parsing data" $ testParseOwnOutput runCompactBinaryGet runCompactBinaryPut compactBinaryV2Data
        ],
        testProperty "check int conversion in CompactBinary" compactDecodeEncodeInt
    ],
    testGroup "Simple Binary protocol" [
        testGroup "Version 1" [
            testCase "parsing existing data" $ testParseCompat runSimpleBinaryV1Get simpleBinaryV1Data,
            testCase "saving and parsing data" $ testParseOwnOutput runSimpleBinaryV1Get runSimpleBinaryV1Put simpleBinaryV1Data
        ],
        testGroup "Version 2" [
            testCase "parsing existing data" $ testParseCompat runSimpleBinaryGet simpleBinaryV2Data,
            testCase "saving and parsing data" $ testParseOwnOutput runSimpleBinaryGet runSimpleBinaryPut simpleBinaryV2Data
        ]
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
    let cv2rec = runGetOrFail runCompactBinaryGet cv2bin
    simple1bin <- BS.readFile simpleBinaryV1Data
    let simple1rec = (runGetOrFail runSimpleBinaryV1Get simple1bin) :: Compat
    simple2bin <- BS.readFile simpleBinaryV2Data
    let simple2rec = runGetOrFail runSimpleBinaryGet simple2bin
    assertEqual "FastBinary read do not match CompactBinary v1 read" fastrec cv1rec
    assertEqual "FastBinary read do not match CompactBinary v2 read" fastrec cv2rec
    assertEqual "SimpleBinary v1 read do not match SimpleBinary v2 read" simple1rec simple2rec
    let simple1bonded = unpackBonded $ m_basicUnintialized simple1rec
    let simple2bonded = unpackBonded $ m_basicUnintialized simple2rec
    let fastbonded = unpackBonded $ m_basicUnintialized fastrec
    assertEqual "FastBinary bonded read do not match SimpleBinary v1 read" fastbonded simple1bonded
    assertEqual "FastBinary bonded read do not match SimpleBinary v2 read" fastbonded simple2bonded
    where
    runGetOrFail get s = case get bondGet s of
                            Right (rest, _, msg) | BS.null rest -> msg
                            Right (_, _, _) -> error "Not all input consumed"
                            Left (_, _, msg) -> error msg

compactDecodeEncodeInt :: Int64 -> Bool
compactDecodeEncodeInt x = EncodedInt x == decodeInt (encodeInt $ EncodedInt x)
