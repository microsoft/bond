{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Unittest.Compat.Compat
import qualified Data.ByteString.Lazy as BS

import Bond.API

main = defaultMain tests

tests = [
    testGroup "Fast Binary protocol" [
        testCase "parsing existing data" testParseCompat,
        testCase "saving and parsing data" testParseOwnOutput
    ]
 ]

runGetOrFail s = case runFastBinaryGet bondGet s of
                    Right (rest, _, msg) | BS.null rest -> msg
                    Right (_, _, _) -> error "Not all input consumed"
                    Left (_, _, msg) -> error "msg"

testParseCompat = do
    b <- BS.readFile "/home/blaze/bond/test/compat/data/compat.fast.dat"
    let parse = runFastBinaryGet bondGet b
    uncurry assertBool $ case parse of
                            Right (rest, _, _ :: Compat) | BS.null rest -> (undefined, True)
                            Right (_, _, _) -> ("Not all input consumed", False)
                            Left (_, _, msg) -> (msg, False)

testParseOwnOutput = do
    b <- BS.readFile "/home/blaze/bond/test/compat/data/compat.fast.dat"
    let s = runGetOrFail b :: Compat
    let b' = runFastBinaryPut (bondPut s)
    let s' = runGetOrFail b' :: Compat
    assertEqual "Saved value do not match parsed one" s s'
