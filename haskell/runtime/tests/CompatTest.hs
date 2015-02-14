{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Unittest.Compat.Compat
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BS

import Bond.API

main = defaultMain tests

tests = [
    testGroup "Fast Binary protocol" [
        testCase "parsing existing data" testParseCompat,
        testCase "saving and parsing data" testParseOwnOutput
    ]
 ]

testParseCompat = do
    b <- BS.readFile "/home/blaze/bond/test/compat/data/compat.fast.dat"
    let parse = runGetOrFail fastBinaryGet b
    uncurry assertBool $ case parse of
                            Right (rest, _, _ :: Compat) | BS.null rest -> (undefined, True)
                            Right (_, _, _) -> ("Not all input consumed", False)
                            Left (_, _, msg) -> (msg, False)

testParseOwnOutput = do
    b <- BS.readFile "/home/blaze/bond/test/compat/data/compat.fast.dat"
    let s = (runGet fastBinaryGet b) :: Compat
    let b' = runPut (fastBinaryPut s)
    let s' = (runGet fastBinaryGet b') :: Compat
    assertEqual "Saved value do not match parsed one" s s'
