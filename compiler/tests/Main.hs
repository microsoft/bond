-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit (testCase)
import Tests.Syntax
import Tests.Codegen

tests :: TestTree
tests = testGroup "Compiler tests"
    [ testGroup "AST"
        [ localOption (QuickCheckMaxSize 15) $
            testProperty "roundtrip" roundtripAST
        , testGroup "Compare .bond and .json"
            [ testCase "attributes" $ compareAST "attributes"
            , testCase "basic types" $ compareAST "basic_types"
            , testCase "complex types" $ compareAST "complex_types"
            , testCase "default values" $ compareAST "defaults"
            , testCase "empty" $ compareAST "empty"
            , testCase "field modifiers" $ compareAST "field_modifiers"
            , testCase "generics" $ compareAST "generics"
            , testCase "inheritance" $ compareAST "inheritance"
            , testCase "type aliases" $ compareAST "aliases"
            , testCase "documentation example" $ compareAST "example"
            ]
        ]
    , testGroup "SchemaDef"
        [ verifySchemaDef "attributes" "Foo"
        , verifySchemaDef "basic_types" "BasicTypes"
        , verifySchemaDef "defaults" "Foo"
        , verifySchemaDef "field_modifiers" "Foo"
        , verifySchemaDef "inheritance" "Foo"
        , verifySchemaDef "alias_key" "foo"
        , verifySchemaDef "maybe_blob" "Foo"
        , verifySchemaDef "nullable_alias" "foo"
        , verifySchemaDef "schemadef" "AliasBase"
        , verifySchemaDef "schemadef" "EnumDefault"
        , verifySchemaDef "schemadef" "StringTree"
        , verifySchemaDef "example" "SomeStruct"
        ]
    , testGroup "Types"
        [ testCase "type alias resolution" aliasResolution
        ]
    , testGroup "Codegen"
        [ testGroup "C++"
            [ verifyCppCodegen "attributes"
            , verifyCppCodegen "basic_types"
            , verifyCppCodegen "complex_types"
            , verifyCppCodegen "defaults"
            , verifyCppCodegen "empty"
            , verifyCppCodegen "field_modifiers"
            , verifyCppCodegen "generics"
            , verifyCppCodegen "inheritance"
            , verifyCppCodegen "aliases"
            , verifyCppCodegen "alias_key"
            , verifyCppCodegen "maybe_blob"
            , verifyCodegen
                [ "c++"
                , "--allocator=arena"
                ]
                "alias_with_allocator"
            , verifyCodegen
                [ "c++"
                , "--allocator=arena"
                , "--using=List=my::list<{0}, arena>"
                , "--using=Vector=my::vector<{0}, arena>"
                , "--using=Set=my::set<{0}, arena>"
                , "--using=Map=my::map<{0}, {1}, arena>"
                , "--using=String=my::string<arena>"
                ]
                "custom_alias_with_allocator"
            , verifyCodegen
                [ "c++"
                , "--allocator=arena"
                , "--using=List=my::list<{0}>"
                , "--using=Vector=my::vector<{0}>"
                , "--using=Set=my::set<{0}>"
                , "--using=Map=my::map<{0}, {1}>"
                , "--using=String=my::string"
                ]
                "custom_alias_without_allocator"
            , verifyApplyCodegen
                [ "c++"
                , "--apply-attribute=DllExport"
                ]
                "basic_types"
            ]
    , testGroup "C#"
            [ verifyCsCodegen "attributes"
            , verifyCsCodegen "basic_types"
            , verifyCsCodegen "complex_types"
            , verifyCsCodegen "defaults"
            , verifyCsCodegen "empty"
            , verifyCsCodegen "field_modifiers"
            , verifyCsCodegen "generics"
            , verifyCsCodegen "inheritance"
            , verifyCsCodegen "aliases"
            , verifyCodegen
                [ "c#"
                , "--using=time=System.DateTime"
                ]
                "nullable_alias"
            ]
        ]
    ]

main :: IO ()
main = defaultMain tests
