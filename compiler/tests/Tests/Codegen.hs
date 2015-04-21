-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE RecordWildCards #-}

module Tests.Codegen
    ( verifyCodegen
    , verifyCppCodegen
    , verifyCsCodegen
    ) where

import System.FilePath
import System.Environment (withArgs)
import Control.Monad
import Data.Monoid
import Prelude
import Data.Text.Lazy (Text, unpack)
import Test.HUnit
import Bond.Template.Util
import Bond.Template.Cpp.Reflection_h
import Bond.Template.Cpp.Types_h
import Bond.Template.Cpp.Apply_h
import Bond.Template.Cpp.Apply_cpp
import Bond.Template.Cpp.Enum_h
import Bond.Template.Cpp.Types_cpp
import Bond.Template.Cs.Types_cs
import Bond.Template.TypeMapping
import Bond.Template.CustomMapping
import Bond.Schema.Types (Bond(..), Import, Declaration)
import Bond.Parser
import Options
import Files
import Tests.Util

type Template = MappingContext -> String -> [Import] -> [Declaration] -> (String, Text)

verifyCppCodegen :: FilePath -> Assertion
verifyCppCodegen = verifyCodegen ["c++"]

verifyCsCodegen :: FilePath -> Assertion
verifyCsCodegen = verifyCodegen ["c#"]

verifyCodegen :: [String] -> FilePath -> Assertion
verifyCodegen args baseName = do
    options <- withArgs args getOptions
    case options of
        Cpp {..} -> verifyCppFiles options baseName
        Cs {..}  -> verifyCsFiles options baseName
        _        -> assert False

verifyCppFiles :: Options -> FilePath -> Assertion
verifyCppFiles options@Cpp {..} = do
    let typeMapping = maybe cppTypeMapping cppCustomAllocTypeMapping allocator
    verifyFiles options typeMapping $
        [ reflection_h
        , types_cpp
        , types_h header enum_header allocator
        , apply_h protocols apply_attribute
        , apply_cpp protocols
        ]
  where
    protocols =
        [ Protocol "CompactBinaryReader" "CompactBinaryWriter"
        , Protocol "FastBinaryReader" "FastBinaryWriter"
        , Protocol "SimpleBinaryReader" "SimpleBinaryWriter"
        ]


verifyCsFiles :: Options -> FilePath -> Assertion
verifyCsFiles options@Cs {..} = do
    let typeMapping = if collection_interfaces then csInterfaceTypeMapping else csTypeMapping
    verifyFiles options typeMapping
        [ types_cs readonly_properties fields
        ]


verifyFiles :: Options -> TypeMapping -> [Template] -> FilePath -> Assertion
verifyFiles options typeMapping templates baseName = do
    aliasMapping <- parseAliasMappings $ using options
    namespaceMapping <- parseNamespaceMappings $ namespace options
    (Bond imports namespaces declarations) <- parseBondFile [] $ "tests" </> "schema" </> baseName <.> "bond"
    let mappingContext = MappingContext typeMapping aliasMapping namespaceMapping namespaces
    forM_ templates $ \template -> do
        let (suffix, code) = template mappingContext baseName imports declarations
        let fileName = baseName ++ suffix
        expected <- readFile $ "tests" </> "generated" </> fileName
        let actual = unpack code
        let msg = "Generated `" ++ fileName ++ "` doesn't match:\n\n" ++ stringDiff actual expected
        assertBool msg (actual == expected)

