-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Tests.Codegen
    ( verifyCodegen
    , verifyCppCodegen
    , verifyApplyCodegen
    , verifyCsCodegen
    ) where

import System.FilePath
import Control.Monad
import Data.Monoid
import Data.Maybe
import Prelude
import Data.Algorithm.DiffContext
import Data.Text.Lazy (Text, unpack)
import qualified Data.ByteString.Char8 as BS
import Text.PrettyPrint (render, text)
import Test.Tasty
import Test.Tasty.Golden.Advanced
import Language.Bond.Codegen.Templates
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Syntax.Types (Bond(..), Import, Declaration)
import Options
import IO

type Template = MappingContext -> String -> [Import] -> [Declaration] -> (String, Text)

verifyCppCodegen :: FilePath -> TestTree
verifyCppCodegen = verifyCodegen ["c++"]

verifyCsCodegen :: FilePath -> TestTree
verifyCsCodegen = verifyCodegen ["c#"]

verifyCodegen :: [String] -> FilePath -> TestTree
verifyCodegen args baseName =
    testGroup baseName $
        verifyFiles (processOptions args) baseName

verifyApplyCodegen :: [String] -> FilePath -> TestTree
verifyApplyCodegen args baseName =
    testGroup baseName $
        map (verifyFile options baseName cppTypeMapping "apply") templates
  where
    options = processOptions args
    templates =
        [ apply_h protocols (apply_attribute options)
        , apply_cpp protocols
        ]
    protocols =
        [ Protocol "bond::CompactBinaryReader<bond::InputBuffer>"
                   "bond::CompactBinaryWriter<bond::OutputBuffer>"
        , Protocol "bond::FastBinaryReader<bond::InputBuffer>"
                   "bond::FastBinaryWriter<bond::OutputBuffer>"
        , Protocol "bond::SimpleBinaryReader<bond::InputBuffer>"
                   "bond::SimpleBinaryWriter<bond::OutputBuffer>"
        ]


verifyFiles :: Options -> FilePath -> [TestTree]
verifyFiles options baseName =
    map (verify (typeMapping options) "") (templates options)
    <>
    extra options
  where
    verify = verifyFile options baseName
    fieldMapping Cs {..} = if readonly_properties
        then ReadOnlyProperties
        else if fields
             then PublicFields
             else Properties
    typeMapping Cpp {..} = maybe cppTypeMapping cppCustomAllocTypeMapping allocator
    typeMapping Cs {} = csTypeMapping
    templates Cpp {..} =
        [ reflection_h
        , types_cpp
        , types_h header enum_header allocator
        ]
    templates Cs {..} =
        [ types_cs Class $ fieldMapping options
        ]
    extra Cs {} =
        [ testGroup "collection interfaces" $
            map (verify csCollectionInterfacesTypeMapping "collection-interfaces") (templates options)
        ]
    extra Cpp {..} =
        [ testGroup "custom allocator" $
            map (verify (cppCustomAllocTypeMapping "arena") "allocator")
                (templates $ options { allocator = Just "arena" })
            | isNothing allocator
        ]

verifyFile :: Options -> FilePath -> TypeMapping -> FilePath -> Template -> TestTree
verifyFile options baseName typeMapping subfolder template =
    goldenTest suffix readGolden codegen cmp updateGolden
  where
    (suffix, _) = template (MappingContext typeMapping [] [] []) "" [] []
    golden = "tests" </> "generated" </> subfolder </> baseName ++ suffix
    readGolden = BS.readFile golden
    updateGolden = BS.writeFile golden
    codegen = do
        aliasMapping <- parseAliasMappings $ using options
        namespaceMapping <- parseNamespaceMappings $ namespace options
        (Bond imports namespaces declarations) <- parseBondFile [] $ "tests" </> "schema" </> baseName <.> "bond"
        let mappingContext = MappingContext typeMapping aliasMapping namespaceMapping namespaces
        let (_, code) = template mappingContext baseName imports declarations
        return $ BS.pack $ unpack code
    cmp x y = return $ if x == y then Nothing else Just $ diff x y
    diff x y = render $ prettyContextDiff
                            (text golden)
                            (text "test output")
                            (text . BS.unpack)
                            (getContextDiff 3 (BS.lines x) (BS.lines y))

