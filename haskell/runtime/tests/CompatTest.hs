module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Data.Int

import Unittest.Compat.Compat
import qualified Data.ByteString.Lazy as BS

import Bond.API
import Bond.Imports (BondStruct, zigzagToWord, wordToZigZag, ZigZagInt(..))

type GetFunc a = BS.ByteString -> Either (BS.ByteString, Int64, String) (BS.ByteString, Int64, a)
type PutFunc a = a -> BS.ByteString

fastBinaryData, compactBinaryV1Data, compactBinaryV2Data, simpleBinaryV1Data, simpleBinaryV2Data :: String
fastBinaryData = "/home/blaze/bond/test/compat/data/compat.fast.dat"
compactBinaryV1Data = "/home/blaze/bond/test/compat/data/compat.compact.dat"
compactBinaryV2Data = "/home/blaze/bond/test/compat/data/compat.compact2.dat"
simpleBinaryV1Data = "/home/blaze/bond/test/compat/data/compat.simple.dat"
simpleBinaryV2Data = "/home/blaze/bond/test/compat/data/compat.simple2.dat"

runGetOrFail :: BondStruct a => GetFunc a -> BS.ByteString -> a
runGetOrFail get s = case get s of
                    Right (rest, _, msg) | BS.null rest -> msg
                    Right (_, _, _) -> error "Not all input consumed"
                    Left (_, _, msg) -> error msg

main :: IO ()
main = defaultMain [
    testGroup "Fast Binary protocol" [
        testCase "parsing existing data" $ testParseCompat deserializeFast fastBinaryData,
        testCase "saving and parsing data" $ testParseOwnOutput deserializeFast serializeFast fastBinaryData,
        testCase "saving and restoring Bonded a" $ testBonded deserializeFast serializeFast fastBinaryData,
        testCase "testing Fast->CompactV1 transform" $ testRecode deserializeFast serializeCompactV1 deserializeCompactV1 fastBinaryData,
        testCase "testing Fast->Compact transform" $ testRecode deserializeFast serializeCompact deserializeCompact fastBinaryData,
        testCase "testing Fast->SimpleV1 transform" $ testRecodeNoDefaults deserializeFast serializeSimpleV1 deserializeSimpleV1 fastBinaryData,
        testCase "testing Fast->Simple transform" $ testRecodeNoDefaults deserializeFast serializeSimple deserializeSimple fastBinaryData
    ],
    testGroup "Compact Binary protocol" [
        testGroup "Version 1" [
            testCase "parsing existing data" $ testParseCompat deserializeCompactV1 compactBinaryV1Data,
            testCase "saving and parsing data" $ testParseOwnOutput deserializeCompactV1 serializeCompactV1 compactBinaryV1Data,
            testCase "saving and restoring Bonded a" $ testBonded deserializeCompactV1 serializeCompactV1 compactBinaryV1Data,
            testCase "testing CompactV1->Fast transform" $ testRecode deserializeCompactV1 serializeFast deserializeFast compactBinaryV1Data,
            testCase "testing CompactV1->CompactV2 transform" $ testRecode deserializeCompactV1 serializeCompact deserializeCompact compactBinaryV1Data,
            testCase "testing CompactV1->SimpleV1 transform" $ testRecodeNoDefaults deserializeCompactV1 serializeSimpleV1 deserializeSimpleV1 compactBinaryV1Data,
            testCase "testing CompactV1->Simple transform" $ testRecodeNoDefaults deserializeCompactV1 serializeSimple deserializeSimple compactBinaryV1Data
        ],
        testGroup "Version 2" [
            testCase "parsing existing data" $ testParseCompat deserializeCompact compactBinaryV2Data,
            testCase "saving and parsing data" $ testParseOwnOutput deserializeCompact serializeCompact compactBinaryV2Data,
            testCase "saving and restoring Bonded a" $ testBonded deserializeCompact serializeCompact compactBinaryV2Data,
            testCase "testing CompactV2->Fast transform" $ testRecode deserializeCompact serializeFast deserializeFast compactBinaryV2Data,
            testCase "testing CompactV2->CompactV1 transform" $ testRecode deserializeCompact serializeCompactV1 deserializeCompactV1 compactBinaryV2Data,
            testCase "testing CompactV2->SimpleV1 transform" $ testRecodeNoDefaults deserializeCompact serializeSimpleV1 deserializeSimpleV1 compactBinaryV2Data,
            testCase "testing CompactV2->Simple transform" $ testRecodeNoDefaults deserializeCompact serializeSimple deserializeSimple compactBinaryV2Data
        ],
        testProperty "check int conversion in CompactBinary" compactDecodeEncodeInt
    ],
    testGroup "Simple Binary protocol" [
        testGroup "Version 1" [
            testCase "parsing existing data" $ testParseCompat deserializeSimpleV1 simpleBinaryV1Data,
            testCase "saving and parsing data" $ testParseOwnOutput deserializeSimpleV1 serializeSimpleV1 simpleBinaryV1Data,
            testCase "saving and restoring Bonded a" $ testBonded deserializeSimpleV1 serializeSimpleV1 simpleBinaryV1Data,
            testCase "testing SimpleV1->CompactV1 transform" $ testRecode deserializeSimpleV1 serializeCompactV1 deserializeCompactV1 simpleBinaryV1Data,
            testCase "testing SimpleV1->Compact transform" $ testRecode deserializeSimpleV1 serializeCompact deserializeCompact simpleBinaryV1Data,
            testCase "testing SimpleV1->Fast transform" $ testRecode deserializeSimpleV1 serializeFast deserializeFast simpleBinaryV1Data,
            testCase "testing SimpleV1->SimpleV2 transform" $ testRecode deserializeSimpleV1 serializeSimple deserializeSimple simpleBinaryV1Data
        ],
        testGroup "Version 2" [
            testCase "parsing existing data" $ testParseCompat deserializeSimple simpleBinaryV2Data,
            testCase "saving and parsing data" $ testParseOwnOutput deserializeSimple serializeSimple simpleBinaryV2Data,
            testCase "saving and restoring Bonded a" $ testBonded deserializeSimple serializeSimple simpleBinaryV2Data,
            testCase "testing Simple->CompactV1 transform" $ testRecode deserializeSimple serializeCompactV1 deserializeCompactV1 simpleBinaryV2Data,
            testCase "testing Simple->Compact transform" $ testRecode deserializeSimple serializeCompact deserializeCompact simpleBinaryV2Data,
            testCase "testing Simple->Fast transform" $ testRecode deserializeSimple serializeFast deserializeFast simpleBinaryV2Data,
            testCase "testing Simple->SimpleV1 transform" $ testRecode deserializeSimple serializeSimpleV1 deserializeSimpleV1 simpleBinaryV2Data
        ]
    ],
    testCase "Check for identical read results" testAllReadSameData
 ]

testParseCompat :: GetFunc Compat -> String -> Assertion
testParseCompat get file = do
    b <- BS.readFile file
    let parse = get b
    uncurry assertBool $ case parse of
                            Right (rest, _, _) | BS.null rest -> (undefined, True)
                            Right (_, _, _) -> ("Not all input consumed", False)
                            Left (_, _, msg) -> (msg, False)

testParseOwnOutput :: GetFunc Compat -> PutFunc Compat -> String -> Assertion
testParseOwnOutput get put file = do
    b <- BS.readFile file
    let s = runGetOrFail get b
    let b' = put s
    let s' = runGetOrFail get b'
    assertEqual "Saved value do not match parsed one" s s'

testBonded :: GetFunc Compat -> PutFunc Compat -> String -> Assertion
testBonded get put file = do
    b <- BS.readFile file
    let s = runGetOrFail get b
    let Right obj = unpackBonded (m_basicUnintialized s)
    let sUnpacked = s { m_basicUnintialized = makeBonded obj }
    let b' = put sUnpacked
    let s' = runGetOrFail get b'
    let Right obj' = unpackBonded (m_basicUnintialized s')
    assertEqual "Saved Bonded value do not match parsed one" obj obj'

testRecode :: GetFunc Compat -> PutFunc Compat -> GetFunc Compat -> String -> Assertion
testRecode get put get' file = do
    b <- BS.readFile file
    let s = runGetOrFail get b
    let b' = put s
    let s' = runGetOrFail get' b'
    assertEqual "Saved object is not equal to original" s s'

testRecodeNoDefaults :: GetFunc Compat -> PutFunc Compat -> GetFunc Compat -> String -> Assertion
testRecodeNoDefaults get put get' file = do
    b <- BS.readFile file
    let s = runGetOrFail get b
    let sfix = s { m_defaults = Nothing } -- simple proto can't save defaultNothing
    let b' = put sfix
    let s' = runGetOrFail get' b'
    assertEqual "Saved object is not equal to original" sfix s'

testAllReadSameData :: Assertion
testAllReadSameData = do
    fastbin <- BS.readFile fastBinaryData
    let fastrec = (runGetOrFail deserializeFast fastbin) :: Compat
    cv1bin <- BS.readFile compactBinaryV1Data
    let cv1rec = runGetOrFail deserializeCompactV1 cv1bin
    cv2bin <- BS.readFile compactBinaryV2Data
    let cv2rec = runGetOrFail deserializeCompact cv2bin
    simple1bin <- BS.readFile simpleBinaryV1Data
    let simple1rec = (runGetOrFail deserializeSimpleV1 simple1bin) :: Compat
    simple2bin <- BS.readFile simpleBinaryV2Data
    let simple2rec = runGetOrFail deserializeSimple simple2bin
    assertEqual "FastBinary read do not match CompactBinary v1 read" fastrec cv1rec
    assertEqual "FastBinary read do not match CompactBinary v2 read" fastrec cv2rec
    assertEqual "SimpleBinary v1 read do not match SimpleBinary v2 read" simple1rec simple2rec
    let simple1bonded = unpackBonded $ m_basicUnintialized simple1rec
    let simple2bonded = unpackBonded $ m_basicUnintialized simple2rec
    let compact1bonded = unpackBonded $ m_basicUnintialized cv1rec
    let compact2bonded = unpackBonded $ m_basicUnintialized cv2rec
    let fastbonded = unpackBonded $ m_basicUnintialized fastrec
    assertEqual "FastBinary bonded read do not match CompactBinary v1 read" fastbonded compact1bonded
    assertEqual "FastBinary bonded read do not match CompactBinary v2 read" fastbonded compact2bonded
    assertEqual "FastBinary bonded read do not match SimpleBinary v1 read" fastbonded simple1bonded
    assertEqual "FastBinary bonded read do not match SimpleBinary v2 read" fastbonded simple2bonded

compactDecodeEncodeInt :: Int64 -> Bool
compactDecodeEncodeInt x = ZigZagInt x == wordToZigZag (zigzagToWord $ ZigZagInt x)
