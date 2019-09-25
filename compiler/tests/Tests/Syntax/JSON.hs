-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Tests.Syntax.JSON
    ( methodParsingTests
    ) where

import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import Data.Aeson.Encode.Pretty (Config(..),NumberFormat(..), Indent(..), encodePretty')
import Data.ByteString.Lazy (ByteString)
import Data.Maybe ()
import Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Language.Bond.Syntax.JSON ()
import Language.Bond.Syntax.Types (Declaration(..), Field(..), Method(..), MethodType(..), Modifier(..), Namespace(..), Type(..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, (@=?), assertFailure, testCase)
import Text.Shakespeare.Text (lt)

methodParsingTests :: TestTree
methodParsingTests = testGroup "Method JSON AST Parsing"
  [ testCase "function without streaming tag: assumed Unary" funcWithoutStreamingTag_assumedUnary
  , testCase "void function(AnyType) unary parses" $ assertJsonDecode (voidFunc Unary) (voidFuncAst "Unary")
  , testCase "void function(AnyType) streaming Client parses" $ assertJsonDecode (voidFunc Streaming) (voidFuncAst "Client")
  , testCase "void function(AnyType) streaming Server fails parsing" $ assertJsonDecodeFails (voidFuncAst "Server") methodDecode
  , testCase "void function(AnyType) streaming Duplex fails parsing" $ assertJsonDecodeFails (voidFuncAst "Duplex") methodDecode
  , testCase "nothing event(AnyType) with any methodStreaming fails parsing" $ assertJsonDecodeFails eventWithStreamingTag methodDecode ]

funcWithoutStreamingTag_assumedUnary :: Assertion
funcWithoutStreamingTag_assumedUnary = assertJsonDecode expected ast
  where
    expected = Function
      { methodAttributes = []
      , methodResult = Unary anyType
      , methodName = "anyMethodName"
      , methodInput = Unary anyType }
    ast = [lt|{
"tag": "Function",
"methodName": "anyMethodName",
"methodInput": #{anyTypeAst},
"methodResult": #{anyTypeAst}
}
|]

-- | Makes a 'Function' that returns void and has the given input type.
voidFunc :: (Type -> MethodType) -> Method
voidFunc streamingType = Function
      { methodAttributes = []
      , methodResult = Void
      , methodName = "anyMethodName"
      , methodInput = (streamingType anyType) }

-- | Makes the JSON AST for a function that returns void with the given
-- "methodStreaming" value.
voidFuncAst :: String -> LT.Text
voidFuncAst streamingTag = [lt|{
"tag": "Function",
"methodName": "anyMethodName",
"methodInput": #{anyTypeAst},
"methodStreaming": "#{streamingTag}"
}
|]

-- | JSON AST of an event that (erroneously) includes a streaming tag.
eventWithStreamingTag :: Text
eventWithStreamingTag = [lt|{
"tag": "Event",
"methodName": "anyMethodName",
"methodInput": #{anyTypeAst},
"methodStreaming": "Unary"
}
|]

-- | JSON AST decoder function for the 'Method' type
methodDecode :: ByteString -> Either String Method
methodDecode = eitherDecode

-- | A placeholder user-defined type
anyType :: Type
anyType = BT_UserDefined
             (Struct
               { declNamespaces = [Namespace { nsLanguage = Nothing, nsName = ["any"] }]
               , declAttributes = []
               , declName = "anyTypeName"
               , declParams = []
               , structBase = Nothing
               , structFields = [Field { fieldAttributes = [], fieldOrdinal = 0, fieldModifier = Optional, fieldType = BT_Int16, fieldName = "anyField", fieldDefault = Nothing }] })
             []

-- | The JSON AST representation of 'anyType'
anyTypeAst :: LT.Text
anyTypeAst = encodeText anyType

-- | Helper method that JSON encodes to lazy text
encodeText :: ToJSON a => a -> LT.Text
encodeText o = decodeUtf8 (encodePretty' config o)
  where config = (Config (Spaces 2)  compare Generic False)

-- | Asserts that the given JSON decodes into the expected value
assertJsonDecode :: (Eq a, FromJSON a, Show a) =>
  a          -- ^ The expected value
  -> LT.Text -- ^ the JSON to decode
  -> Assertion
assertJsonDecode expected json = case eitherDecode (encodeUtf8 json) of
  Left parseError -> assertFailure ("JSON parse error: " ++ parseError ++ "\nInput JSON:\n" ++ (LT.unpack json))
  Right actual -> expected @=? actual

-- | Asserts that attempting to decode the provided 'json' with the provided
-- 'decode' function fails.
assertJsonDecodeFails :: Show a =>
  LT.Text                            -- ^ the JSON to decode
  -> (ByteString -> Either String a) -- ^ the decode function to run. Usually 'eitherDecode' for a specific type
  -> Assertion
assertJsonDecodeFails json decode = case decode (encodeUtf8 json) of
  Left _ -> return ()
  Right o -> assertFailure ("Expected JSON parsing to fail, but got '" ++ show o ++ "' instead.")
