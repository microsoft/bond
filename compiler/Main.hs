-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE RecordWildCards #-}

import System.Environment (getArgs, withArgs)
import System.Directory
import System.FilePath
import System.IO
import System.Exit
import Control.Applicative
import Data.Monoid
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Loops (firstM)
import Control.Concurrent.Async
import GHC.Conc (getNumProcessors, setNumCapabilities)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as L
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import Bond.Schema.Types (Bond(..))
import Bond.Schema.JSON
import Bond.Parser
import Bond.Schema.Types (Declaration, Import)
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
import Options

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
        BL.writeFile (output_dir </> fileName <.> "json") $ encode bond

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
    let typeMapping = if collection_interfaces then csInterfaceTypeMapping else csTypeMapping
    concurrentlyFor_ files $ codeGen options typeMapping
        [ types_cs readonly_properties fields
        ]
csCodegen _ = error "csCodegen: impossible happened."

codeGen :: Options -> TypeMapping -> [Template] -> FilePath -> IO ()
codeGen options typeMapping templates file = do
    let outputDir = output_dir options
    let baseName = takeBaseName file
    aliasMapping <- parseAliasMappings $ using options
    namespaceMapping <- parseNamespaceMappings $ namespace options
    (Bond imports namespaces declarations) <- parseFile (import_dir options) file
    forM_ templates $ \template -> do
        let mappingContext = MappingContext typeMapping aliasMapping namespaceMapping namespaces
        let (suffix, code) = template mappingContext baseName imports declarations
        let fileName = baseName ++ suffix
        createDirectoryIfMissing True outputDir
        L.writeFile (outputDir </> fileName) (commonHeader fileName <> code)


parseFile :: [FilePath] -> FilePath -> IO(Bond)
parseFile importDirs file = do
    cwd <- getCurrentDirectory
    input <- readFileUtf8 file
    result <- runReaderT (parseBond file input) (newEnvironment (cwd </> file) (readImportFile importDirs))
    case result of
        Left error -> do
            print error
            exitFailure
        Right bond -> return bond


readImportFile :: [FilePath] -> FilePath -> FilePath -> IO (FilePath, String)
readImportFile importDirs parentFile file = do
    path <- findFilePath (takeDirectory parentFile:importDirs)
    case path of
        Just path -> do
            content <- readFileUtf8 path
            return (path, content)
        Nothing -> fail $ "Can't find import file " ++ file
  where
    findFilePath dirs = fmap (</> file) <$> firstM (doesFileExist . (</> file)) dirs
                              
readFileUtf8 :: FilePath -> IO String
readFileUtf8 name = do 
    h <- openFile name ReadMode
    hSetEncoding h utf8_bom
    hGetContents h

