-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE RecordWildCards #-}

import System.Environment (getArgs, withArgs)
import System.Directory
import System.FilePath
import Data.Monoid
import Control.Monad
import Prelude
import Control.Concurrent.Async
import GHC.Conc (getNumProcessors, setNumCapabilities)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as L
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import Language.Bond.Syntax.Types (Bond(..), Declaration(..), Import, Type(..))
import Language.Bond.Syntax.JSON()
import Language.Bond.Syntax.SchemaDef
import Language.Bond.Codegen.Util
import Language.Bond.Codegen.Templates
import Language.Bond.Codegen.TypeMapping
import Options
import IO

type Template = MappingContext -> String -> [Import] -> [Declaration] -> (String, Text)

main :: IO()
main = do
    args <- getArgs
    options <- (if null args then withArgs ["--help"] else id) getOptions
    setJobs $ jobs options
    case options of
        Cpp {..}    -> cppCodegen options
        Cs {..}     -> csCodegen options
        Schema {..} -> writeSchema options
        _           -> print options

setJobs :: Maybe Int -> IO ()
setJobs Nothing = return ()
setJobs (Just n)
    | n > 0     = setNumCapabilities n
    | otherwise = do
        numProc <- getNumProcessors
        -- if n is less than 0 use that many fewer jobs than processors
        setNumCapabilities $ max 1 (numProc + n)

concurrentlyFor_ :: [a] -> (a -> IO b) -> IO ()
concurrentlyFor_ = (void .) . flip mapConcurrently


writeSchema :: Options -> IO()
writeSchema Schema {..} =
    concurrentlyFor_ files $ \file -> do
        let fileName = takeBaseName file
        bond <- parseFile import_dir file
        if runtime_schema then
                forM_ (bondDeclarations bond) (writeSchemaDef fileName)
            else
                BL.writeFile (output_dir </> fileName <.> "json") $ encode bond
  where
    writeSchemaDef fileName s@Struct{..} | null declParams =
        BL.writeFile (output_dir </> fileName <.> declName <.> "json") $ encodeSchemaDef $ BT_UserDefined s []
    writeSchemaDef _ _ = return ()

writeSchema _ = error "writeSchema: impossible happened."

cppCodegen :: Options -> IO()
cppCodegen options@Cpp {..} = do
    let typeMapping = maybe cppTypeMapping cppCustomAllocTypeMapping allocator
    concurrentlyFor_ files $ codeGen options typeMapping $
        [ reflection_h
        , types_cpp
        , types_h header enum_header allocator
        , apply_h applyProto apply_attribute
        , apply_cpp applyProto
        ] <>
        [ enum_h | enum_header]
  where
    applyProto = map snd $ filter (enabled apply) protocols
    enabled a p = null a || fst p `elem` a
    protocols =
        [ (Compact, Protocol "CompactBinaryReader" "CompactBinaryWriter")
        , (Fast, Protocol "FastBinaryReader" "FastBinaryWriter")
        , (Simple, Protocol "SimpleBinaryReader" "SimpleBinaryWriter")
        ]
cppCodegen _ = error "cppCodegen: impossible happened."


csCodegen :: Options -> IO()
csCodegen options@Cs {..} = do
    let fieldMapping = if readonly_properties
            then ReadOnlyProperties
            else if fields
                 then PublicFields
                 else Properties
    let typeMapping = if collection_interfaces then csCollectionInterfacesTypeMapping else csTypeMapping
    let templates = [ types_cs Class fieldMapping ]
    concurrentlyFor_ files $ codeGen options typeMapping templates
csCodegen _ = error "csCodegen: impossible happened."

codeGen :: Options -> TypeMapping -> [Template] -> FilePath -> IO ()
codeGen options typeMapping templates file = do
    let outputDir = output_dir options
    let baseName = takeBaseName file
    aliasMapping <- parseAliasMappings $ using options
    namespaceMapping <- parseNamespaceMappings $ namespace options
    (Bond imports namespaces declarations) <- parseFile (import_dir options) file
    let mappingContext = MappingContext typeMapping aliasMapping namespaceMapping namespaces
    forM_ templates $ \template -> do
        let (suffix, code) = template mappingContext baseName imports declarations
        let fileName = baseName ++ suffix
        createDirectoryIfMissing True outputDir
        let content = if (no_banner options) then code else (commonHeader "//" fileName <> code)
        L.writeFile (outputDir </> fileName) content

