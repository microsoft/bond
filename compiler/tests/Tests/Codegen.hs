-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Tests.Codegen
    ( verifyCodegen
    , verifyCodegenVariation
    , verifyCppCodegen
    , verifyApplyCodegen
    , verifyExportsCodegen
    , verifyCsCodegen
    , verifyJavaCodegen
    ) where

import System.FilePath
import Control.Monad
import Data.Monoid
import Data.Maybe
import Prelude
import Data.Algorithm.DiffContext
import Data.Text.Lazy (Text, unpack)
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Char8 as BS
import Text.PrettyPrint (render, text)
import Test.Tasty
import Test.Tasty.Golden.Advanced
import Language.Bond.Codegen.Templates
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Syntax.Types (Bond(..), Import, Declaration(..))
import Options
import IO

type Template = MappingContext -> String -> [Import] -> [Declaration] -> (String, Text)

verifyCppCodegen :: FilePath -> TestTree
verifyCppCodegen = verifyCodegen ["c++"]

verifyCsCodegen :: FilePath -> TestTree
verifyCsCodegen = verifyCodegen ["c#"]

verifyJavaCodegen :: FilePath -> TestTree
verifyJavaCodegen = verifyCodegen ["java"]

verifyCodegen :: [String] -> FilePath -> TestTree
verifyCodegen args baseName =
    testGroup baseName $
        verifyFiles (processOptions args) baseName ""

verifyCodegenVariation :: [String] -> FilePath -> FilePath -> TestTree
verifyCodegenVariation args baseName variation =
    testGroup baseName $
        verifyFiles (processOptions args) baseName variation

verifyApplyCodegen :: [String] -> FilePath -> TestTree
verifyApplyCodegen args baseName =
    testGroup baseName $
        map (verifyFile options baseName cppTypeMapping "apply") templates
  where
    options = processOptions args
    templates =
        [ apply_h protocols (export_attribute options)
        , apply_cpp protocols
        ]
    protocols =
        [ ProtocolReader "bond::CompactBinaryReader<bond::InputBuffer>"
        , ProtocolWriter "bond::CompactBinaryWriter<bond::OutputBuffer>"
        , ProtocolWriter "bond::CompactBinaryWriter<bond::OutputCounter>"
        , ProtocolReader "bond::FastBinaryReader<bond::InputBuffer>"
        , ProtocolWriter "bond::FastBinaryWriter<bond::OutputBuffer>"
        , ProtocolReader "bond::SimpleBinaryReader<bond::InputBuffer>"
        , ProtocolWriter "bond::SimpleBinaryWriter<bond::OutputBuffer>"
        ]

verifyExportsCodegen :: [String] -> FilePath -> TestTree
verifyExportsCodegen args baseName =
    testGroup baseName $
        map (verifyFile options baseName (cppExpandAliases (type_aliases_enabled options) cppTypeMapping) "exports") (templates options)
  where
    options = processOptions args
    templates Cpp {..} =
        [ reflection_h export_attribute
        , types_h export_attribute header enum_header allocator alloc_ctors_enabled type_aliases_enabled scoped_alloc_enabled
        ]

verifyFiles :: Options -> FilePath -> FilePath -> [TestTree]
verifyFiles options baseName variation =
    map (verify (typeMapping options) variation) (templates options)
    <>
    extra options
  where
    verify = verifyFile options baseName
    fieldMapping Cs {..} = if readonly_properties
        then ReadOnlyProperties
        else if fields
             then PublicFields
             else Properties
    constructorOptions Cs {..} = if constructor_parameters
        then ConstructorParameters
        else DefaultWithProtectedBase
    typeMapping Cpp {..} = cppExpandAliases type_aliases_enabled $ maybe cppTypeMapping (cppCustomAllocTypeMapping scoped_alloc_enabled) allocator
    typeMapping Cs {} = csTypeMapping
    typeMapping Java {} = javaTypeMapping
    templates Cpp {..} =
        [ (reflection_h export_attribute)
        , types_cpp
        , types_h export_attribute header enum_header allocator alloc_ctors_enabled type_aliases_enabled scoped_alloc_enabled
        ] <>
        [ enum_h | enum_header]
    templates Cs {..} =
        [ types_cs Class (fieldMapping options) (constructorOptions options)
        ]
    templates Java {} =
        [ javaCatTemplate
        ]
    extra Cs {} =
        [ testGroup "collection interfaces" $
            map (verify csCollectionInterfacesTypeMapping (variation </> "collection-interfaces")) (templates options)
        ]
    extra Cpp {..} =
        [ testGroup "custom allocator" $
            map (verify (cppExpandAliasesTypeMapping $ cppCustomAllocTypeMapping False "arena") (variation </> "allocator"))
                (templates $ options { allocator = Just "arena" })
            | isNothing allocator
        ] ++
        [ testGroup "constructors with allocator argument" $
            map (verify (cppExpandAliasesTypeMapping $ cppCustomAllocTypeMapping False "arena") (variation </> "alloc_ctors"))
                (templates $ options { allocator = Just "arena", alloc_ctors_enabled = True })
            | isNothing allocator
        ] ++
        [ testGroup "type aliases" $
            map (verify (cppCustomAllocTypeMapping False "arena") (variation </> "type_aliases"))
                (templates $ options { allocator = Just "arena", type_aliases_enabled = True })
        ] ++
        [ testGroup "scoped allocator" $
            map (verify (cppExpandAliasesTypeMapping $ cppCustomAllocTypeMapping True "arena") (variation </> "scoped_allocator"))
                (templates $ options { allocator = Just "arena", scoped_alloc_enabled = True })
            | isNothing allocator
        ]
    extra Java {} =
        [
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
        (Bond imports namespaces declarations) <- parseBondFile (import_dir options) $ "tests" </> "schema" </> baseName <.> "bond"
        let mappingContext = MappingContext typeMapping aliasMapping namespaceMapping namespaces
        let (_, code) = template mappingContext baseName imports declarations
        return $ BS.pack $ unpack code
    cmp x y = return $ if x == y then Nothing else Just $ diff x y
    diff x y = render $ prettyContextDiff
                            (text golden)
                            (text "test output")
                            (text . BS.unpack)
                            (getContextDiff 3 (BS.lines x) (BS.lines y))

javaCatTemplate :: MappingContext -> String -> [Import] -> [Declaration] -> (String, Text)
javaCatTemplate mappingContext _ imports declarations =
  (suffix, LT.concat $ mapMaybe codegenDecl declarations)
    where
      suffix = "_concatenated.java"
      codegenDecl declaration =
        case declaration of
          Struct {} -> Just $ class_java mappingContext imports declaration
          Enum {}   -> Just $ enum_java mappingContext declaration
          _         -> Nothing

cppExpandAliases :: Bool -> TypeMapping -> TypeMapping
cppExpandAliases type_aliases_enabled = if type_aliases_enabled
    then id
    else cppExpandAliasesTypeMapping
