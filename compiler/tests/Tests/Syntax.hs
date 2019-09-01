-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE OverloadedStrings, RecordWildCards, TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.Syntax
    ( roundtripAST
    , compareAST
    , failBadSyntax
    , aliasResolution
    , verifySchemaDef
    ) where

import Control.Exception
import Data.Maybe
import Data.List
import Data.Aeson (encode, decode)
import Data.Aeson.Encode.Pretty (Config(..), NumberFormat(..), Indent(..), encodePretty')
import Test.QuickCheck.TH.Generators
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import System.FilePath
import Test.QuickCheck
import Test.HUnit
import Test.Tasty
import Test.Tasty.Golden
import Language.Bond.Syntax.Types
import Language.Bond.Syntax.Util
import Language.Bond.Syntax.SchemaDef
import IO

makeArbitrary ''Attribute
makeArbitrary ''Constant
makeArbitrary ''Constraint
makeArbitrary ''Default
makeArbitrary ''Field
makeArbitrary ''Declaration
makeArbitrary ''Import
makeArbitrary ''Language
makeArbitrary ''Modifier
makeArbitrary ''Namespace
makeArbitrary ''Type
makeArbitrary ''TypeParam
makeArbitrary ''MethodType
makeArbitrary ''Bond


instance Arbitrary Attribute where
  arbitrary = arbitraryAttribute

instance Arbitrary Field where
  arbitrary = arbitraryField

instance Arbitrary Constant where
  arbitrary = arbitraryConstant

instance Arbitrary Default where
  arbitrary = arbitraryDefault

instance Arbitrary Declaration where
  arbitrary = arbitraryDeclaration

instance Arbitrary Import where
  arbitrary = arbitraryImport

instance Arbitrary Constraint where
  arbitrary = arbitraryConstraint

instance Arbitrary Language where
  arbitrary = arbitraryLanguage

instance Arbitrary Modifier where
  arbitrary = arbitraryModifier


instance Arbitrary Namespace where
  arbitrary = arbitraryNamespace

instance Arbitrary TypeParam where
  arbitrary = arbitraryTypeParam

instance Arbitrary Type where
  arbitrary = arbitraryType

instance Arbitrary MethodType where
  arbitrary = arbitraryMethodType

instance Arbitrary Bond where
  arbitrary = arbitraryBond


instance Arbitrary Method where
  arbitrary = oneof
    [ Function <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , Event <$> arbitrary <*> arbitrary <*> eventInput]
    where
      -- events cannot have streaming input, so we need to customize the
      -- arbitrary input to omit Streaming
      eventInput = oneof [return Void,  Unary <$> arbitrary]

roundtripAST :: Bond -> Property
roundtripAST x = (decode . encode) x === Just x

assertException :: String -> IO a -> Assertion
assertException errMsg action = do
    e <- try action
    case e of
        Left (_ :: SomeException) -> return ()
        Right _ -> assertFailure errMsg

failBadSyntax :: String -> FilePath -> Assertion
failBadSyntax errMsg file = assertException errMsg (parseBondFile [] $ "tests" </> "schema" </> "error" </> file <.> "bond")

compareAST :: FilePath -> Assertion
compareAST file = do
    bond <- parseBondFile [] $ "tests" </> "schema" </> file <.> "bond"
    json <- parseASTFile $ "tests" </> "schema" </> file <.> "json"
    assertEqual "" bond json

aliasResolution :: Assertion
aliasResolution = do
    let p1 = TypeParam "T" Nothing
    let p2 = TypeParam "U" Nothing
    let a1 = Alias [] "" [] BT_Bool
    let a2 = Alias [] "" [p1] $ BT_TypeParam p1
    let a3 = Alias [] "" [p1] $ BT_List $ BT_TypeParam p1
    let a4 = Alias [] "" [p1, p2] $ BT_Nullable $ BT_Map (BT_TypeParam p2) (BT_Nullable $ BT_TypeParam p1)
    let a5 = Alias [] "" [p1] $ BT_List $ BT_UserDefined a3 [BT_TypeParam p1]
    let a6 = Alias [] "" [p1, p2] $ BT_Vector $ BT_Vector $ BT_TypeParam p2

    assertEqual "" BT_Bool $
        resolveAlias a1 []

    assertEqual "" BT_Int32 $
        resolveAlias a2 [BT_Int32]

    assertEqual "" (BT_Set BT_Int32) $
        resolveAlias a2 [BT_Set BT_Int32]

    assertEqual "" (BT_TypeParam p2) $
        resolveAlias a2 [BT_TypeParam p2]

    assertEqual "" (BT_TypeParam p1) $
        resolveAlias a2 [BT_TypeParam p1]

    assertEqual "" (BT_List BT_Int32) $
        resolveAlias a3 [BT_Int32]

    assertEqual "" (BT_List $ BT_UserDefined a1 []) $
        resolveAlias a3 [BT_UserDefined a1 []]

    assertEqual "" (BT_Nullable $ BT_Map BT_Bool $ BT_Nullable BT_String) $
        resolveAlias a4 [BT_String, BT_Bool]

    assertEqual "" (BT_List $ BT_UserDefined a3 [BT_Set BT_Bool]) $
        resolveAlias a5 [BT_Set BT_Bool]

    assertEqual "" (BT_Vector $ BT_Vector $ BT_List BT_Double) $
        resolveAlias a6 [BT_IntTypeArg 10, BT_List BT_Double]

verifySchemaDef :: FilePath -> String -> TestTree
verifySchemaDef baseName schemaName =
    goldenVsString
        schemaName
        ("tests" </> "generated" </> "schemadef" </> baseName <.> schemaName <.> "json")
        schemaDef
  where
    schemaDef = do
        (Bond _ _ declarations) <- parseBondFile [] $ "tests" </> "schema" </> baseName <.> "bond"
        let schema = fromJust $ find ((schemaName ==) . declName) declarations
        return $
            BL.fromStrict $
            -- aeson-pretty-print prints anything above 1e19 in scientific notation
            replace "1.8446744073709551615e19" "18446744073709551615" $
            -- some versions aeson encode angle brackets
            replace "\\u003c" "<" $
            replace "\\u003e" ">" $
            BL.toStrict $ prettyEncode $ makeSchemaDef $ BT_UserDefined schema []
      where
        prettyEncode = encodePretty' (Config indentSpaces compare Generic False)
          where
            indentSpaces = Spaces 2
        replace s r bs = if B.null t then h else
            B.append h (B.append r $ replace s r (B.drop (B.length s) t))
          where
            (h, t) = B.breakSubstring s bs
