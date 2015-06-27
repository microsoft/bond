-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE RecordWildCards #-}

module Tests.Codegen
    ( verifyCodegen
    , verifyCppCodegen
    , verifyApplyCodegen
    , verifyCsCodegen
    ) where

import System.FilePath
import System.Environment (withArgs)
import Control.Monad
import Data.Monoid
import Data.Maybe
import Prelude
import Data.Text.Lazy (Text, unpack)
import Test.HUnit
import Language.Bond.Codegen.Util
import Language.Bond.Codegen.Templates
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.CustomMapping
import Language.Bond.Syntax.Types (Bond(..), Import, Declaration)
import Language.Bond.Parser
import Options
import IO
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
verifyCppFiles options@Cpp {..} baseName = do
    let typeMapping = maybe cppTypeMapping cppCustomAllocTypeMapping allocator
    let templates _allocator =
            [ reflection_h
            , types_cpp
            , types_h header enum_header _allocator
            ]
    let verify = verifyFiles options baseName
    verify (templates allocator) typeMapping ""
    when (isNothing allocator) $
        verify (templates $ Just "arena") (cppCustomAllocTypeMapping "arena") "allocator"


verifyApplyCodegen :: [String] -> FilePath -> Assertion
verifyApplyCodegen args baseName = do
    options <- withArgs args getOptions
    let templates =
            [ apply_h protocols (apply_attribute options)
            , apply_cpp protocols
            ]
    verifyFiles options baseName templates cppTypeMapping "apply"
  where
    protocols =
        [ Protocol "CompactBinaryReader" "CompactBinaryWriter"
        , Protocol "FastBinaryReader" "FastBinaryWriter"
        , Protocol "SimpleBinaryReader" "SimpleBinaryWriter"
        ]

verifyCsFiles :: Options -> FilePath -> Assertion
verifyCsFiles options@Cs {..} baseName = do
    let typeMapping = if collection_interfaces then csInterfaceTypeMapping else csTypeMapping
    let verify = verifyFiles options baseName
            [ types_cs readonly_properties fields
            ]
    verify typeMapping ""
    unless collection_interfaces $
        verify csInterfaceTypeMapping "collection-interfaces"


verifyFiles :: Options -> FilePath -> [Template] -> TypeMapping -> FilePath -> Assertion
verifyFiles options baseName templates typeMapping subfolder = do
    aliasMapping <- parseAliasMappings $ using options
    namespaceMapping <- parseNamespaceMappings $ namespace options
    (Bond imports namespaces declarations) <- parseBondFile [] $ "tests" </> "schema" </> baseName <.> "bond"
    let mappingContext = MappingContext typeMapping aliasMapping namespaceMapping namespaces
    forM_ templates $ \template -> do
        let (suffix, code) = template mappingContext baseName imports declarations
        let fileName = subfolder </> baseName ++ suffix
        expected <- readFile $ "tests" </> "generated" </> fileName
        let actual = unpack code
        let msg = "Generated `" ++ fileName ++ "` doesn't match:\n\n" ++ stringDiff actual expected
        assertBool msg (actual == expected)

