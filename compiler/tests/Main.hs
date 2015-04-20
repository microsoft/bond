-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Tests.Schema
import Tests.Codegen

tests :: [Test]
tests =
    [ testGroup "AST"
        [ testProperty "roundtrip" roundtripAST
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
            ]
        ]
    , testGroup "Codegen"
        [ testGroup "C++"
            [ testCase "attributes" $ verifyCppCodegen "attributes"
            , testCase "basic types" $ verifyCppCodegen "basic_types"
            , testCase "complex types" $ verifyCppCodegen "complex_types"
            , testCase "default values" $ verifyCppCodegen "defaults"
            , testCase "empty" $ verifyCppCodegen "empty"
            , testCase "field modifiers" $ verifyCppCodegen "field_modifiers"
            , testCase "generics" $ verifyCppCodegen "generics"
            , testCase "inheritance" $ verifyCppCodegen "inheritance"
            , testCase "type aliases" $ verifyCppCodegen "aliases"
            ]
        , testGroup "C#"
            [ testCase "attributes" $ verifyCsCodegen "attributes"
            , testCase "basic types" $ verifyCsCodegen "basic_types"
            , testCase "complex types" $ verifyCsCodegen "complex_types"
            , testCase "default values" $ verifyCsCodegen "defaults"
            , testCase "empty" $ verifyCsCodegen "empty"
            , testCase "field modifiers" $ verifyCsCodegen "field_modifiers"
            , testCase "generics" $ verifyCsCodegen "generics"
            , testCase "inheritance" $ verifyCsCodegen "inheritance"
            , testCase "type aliases" $ verifyCsCodegen "aliases"
            ]
        ]
    ]

main :: IO ()
main = do
    options <- getArgs >>= interpretArgs
    case options of
        Left error -> do
            putStr error
            exitFailure
        Right (runnerOptions, _) -> do
            let testOptions = maybe mempty id $ ropt_test_options runnerOptions
            let testSize = topt_maximum_test_size testOptions
            defaultMainWithOpts tests $ runnerOptions
                { ropt_test_options = Just testOptions
                    -- Use a smaller maximum test size by default
                    { topt_maximum_test_size = Just $ maybe 15 id testSize
                    }
                }
