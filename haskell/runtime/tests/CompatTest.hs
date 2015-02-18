module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Int

import Unittest.Compat.Compat
import qualified Data.ByteString.Lazy as BS

import Bond.API

type GetFunc t a = BondGet t a -> BS.ByteString -> Either (BS.ByteString, Int64, String) (BS.ByteString, Int64, a)
type PutFunc t = BondPut t -> BS.ByteString

main :: IO ()
main = defaultMain [
    testGroup "Fast Binary protocol" [
        testCase "parsing existing data" $ testParseCompat runFastBinaryGet "/home/blaze/bond/test/compat/data/compat.fast.dat",
        testCase "saving and parsing data" $ testParseOwnOutput runFastBinaryGet runFastBinaryPut "/home/blaze/bond/test/compat/data/compat.fast.dat"
    ],
    testGroup "Compact Binary protocol" [
        testGroup "Version 2" [
            testCase "parsing existing data" $ testParseCompat runCompactBinaryV2Get "/home/blaze/bond/test/compat/data/compat.compact2.dat",
            testCase "saving and parsing data" $ testParseOwnOutput runCompactBinaryV2Get runCompactBinaryV2Put "/home/blaze/bond/test/compat/data/compat.compact2.dat"
        ]
    ]
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

