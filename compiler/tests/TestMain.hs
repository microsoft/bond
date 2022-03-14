-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit (testCase)
import Tests.Syntax
import Tests.Syntax.JSON(methodParsingTests)
import Tests.Codegen
import Tests.Codegen.Util(utilTestGroup)

tests :: TestTree
tests = testGroup "Compiler tests"
    [ testGroup "AST"
        [ localOption (QuickCheckMaxSize 6) $
            testProperty "roundtrip" roundtripAST
        , testGroup "Compare .bond and .json"
            [ testCase "attributes" $ compareAST "attributes"
            , testCase "basic types" $ compareAST "basic_types"
            , testCase "bond_meta types" $ compareAST "bond_meta"
            , testCase "complex types" $ compareAST "complex_types"
            , testCase "default values" $ compareAST "defaults"
            , testCase "empty" $ compareAST "empty"
            , testCase "field modifiers" $ compareAST "field_modifiers"
            , testCase "generics" $ compareAST "generics"
            , testCase "inheritance" $ compareAST "inheritance"
            , testCase "type aliases" $ compareAST "aliases"
            , testCase "documentation example" $ compareAST "example"
            , testCase "simple service syntax" $ compareAST "service"
            , testCase "service attributes" $ compareAST "service_attributes"
            , testCase "generic service" $ compareAST "generic_service"
            , testCase "streaming service" $ compareAST "streaming"
            , testCase "documentation example" $ compareAST "example"
            , testCase "service inheritance" $ compareAST "service_inheritance"
            ]
        , methodParsingTests
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
    , testGroup "Codegen Failures (Expect to see errors below check for OK or FAIL)"
        [ testCase "Struct default value nothing" $ failBadSyntax "Should fail when default value of a struct field is 'nothing'" "struct_nothing"
        , testCase "Enum no default value" $ failBadSyntax "Should fail when an enum field has no default value" "enum_no_default"
        , testCase "Alias default value" $ failBadSyntax "Should fail when underlying default value is of the wrong type" "aliases_default"
        , testCase "Out of range" $ failBadSyntax "Should fail, out of range for int16" "int_out_of_range"
        , testCase "Duplicate method definition in service" $ failBadSyntax "Should fail, method name should be unique" "duplicate_service_method"
        , testCase "Invalid service base: struct" $ failBadSyntax "Should fail, struct can't be used as service base" "service_invalid_base_struct"
        , testCase "Invalid service base: type param" $ failBadSyntax "Should fail, type param can't be used as service base" "service_invalid_base_type_param"
        ]
    , testGroup "Codegen"
        [ utilTestGroup,
          testGroup "C++"
            [ verifyCppCodegen "attributes"
            , verifyCppCodegen "basic_types"
            , verifyCppCodegen "bond_meta"
            , verifyCppCodegen "complex_types"
            , verifyCppCodegen "defaults"
            , verifyCppCodegen "empty"
            , verifyCppCodegen "field_modifiers"
            , verifyCppCodegen "generics"
            , verifyCppCodegen "inheritance"
            , verifyCppCodegen "aliases"
            , verifyCppCodegen "alias_key"
            , verifyCppCodegen "maybe_blob"
            , verifyCppCodegen "metadata_edge_cases"
            , verifyCodegen
                [ "c++"
                , "--enum-header"
                ]
                "with_enum_header"
            , verifyCodegen
                 [ "c++"
                 , "--import-dir=tests/schema/imports"
                 ]
                 "import"
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
           , testGroup "Apply"
                [ verifyApplyCodegen
                    [ "c++"
                    , "--apply-attribute=DllExport"
                    ]
                    "basic_types"
                ]
           , testGroup "Exports"
                [ verifyExportsCodegen
                    [ "c++"
                    , "--export-attribute=DllExport"
                    ]
                    "with_enum_header"
                ]
            , verifyCodegen
                [ "c++"
                , "--namespace=tests=nsmapped"
                ]
                "basic_types_nsmapped"
            ]
        , testGroup "C#"
            [ verifyCsCodegen "attributes"
            , verifyCsCodegen "basic_types"
            , verifyCsCodegen "bond_meta"
            , verifyCsCodegen "complex_types"
            , verifyCsCodegen "defaults"
            , verifyCsCodegen "empty"
            , verifyCsCodegen "field_modifiers"
            , verifyCsCodegen "generics"
            , verifyCsCodegen "inheritance"
            , verifyCsCodegen "aliases"
            , verifyCsCodegen "complex_inheritance"
            , verifyCodegenVariation
                [ "c#"
                , "--preview-constructor-parameters"
                , "--readonly-properties"
                ]
                "complex_inheritance"
                "constructor-parameters"
            , verifyCodegenVariation
                [ "c#"
                , "--preview-constructor-parameters"
                , "--fields"
                ]
                "complex_inheritance"
                "constructor-parameters_fields"
            , verifyCodegen
                [ "c#"
                , "--using=time=System.DateTime"
                ]
                "nullable_alias"
            , verifyCodegen
                [ "c#"
                , "--namespace=tests=nsmapped"
                ]
                "basic_types_nsmapped"
            , verifyCodegen
                 [ "c#"
                 , "--import-dir=tests/schema/imports"
                 ]
                 "import"
            , verifyCodegenVariation
                [ "c#"
                , "--preview-constructor-parameters"
                , "--readonly-properties"
                ]
                "empty_struct"
                "constructor-parameters"
            ]
        , testGroup "Java"
            [ verifyJavaCodegen "attributes"
            , verifyJavaCodegen "basic_types"
            , verifyJavaCodegen "bond_meta"
            , verifyJavaCodegen "complex_types"
            , verifyJavaCodegen "defaults"
            , verifyJavaCodegen "empty"
            , verifyJavaCodegen "field_modifiers"
            , verifyJavaCodegen "generics"
            , verifyJavaCodegen "inheritance"
            , verifyJavaCodegen "aliases"
            , verifyCodegen
                [ "java"
                , "--namespace=tests=nsmapped"
                ]
                "basic_types_nsmapped"
            , verifyCodegen
                 [ "java"
                 , "--import-dir=tests/schema/imports"
                 ]
                 "import"
            ]
        ]
    ]

main :: IO ()
main = defaultMain tests
