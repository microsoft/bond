-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

module IO
    ( parseFile
    , parseBondFile
    , parseASTFile
    , parseNamespaceMappings
    , parseAliasMappings
    )
    where

import System.Exit
import System.FilePath
import System.Directory
import System.IO
import Control.Applicative
import Prelude
import Data.Aeson (eitherDecode)
import Data.Text
import Control.Monad.Loops (firstM)
import qualified Data.ByteString.Lazy as BL
import Text.Parsec
import Text.ParserCombinators.Parsec.Error
import Text.Printf
import Language.Bond.Syntax.Types (Bond(..))
import Language.Bond.Syntax.JSON()
import Language.Bond.Parser
import Language.Bond.Codegen.TypeMapping


parseFile :: [FilePath] -> FilePath -> IO(Bond)
parseFile importDirs file =
    if takeExtension file == ".json" then
        parseASTFile file else
        parseBondFile importDirs file


parseBondFile :: [FilePath] -> FilePath -> IO(Bond)
parseBondFile importDirs file = do
    cwd <- getCurrentDirectory
    input <- readFileUtf8 file
    result <- parseBond file input (cwd </> file) readImportFile
    case result of
        Left err -> do
            putStrLn $ msbuildErrorMessage err
            exitFailure
        Right bond -> return bond
  where
    readImportFile parentFile importFile = do
        path <- findFilePath (takeDirectory parentFile:importDirs)
        case path of
            Just path' -> do
                content <- readFileUtf8 path'
                return (path', content)
            Nothing -> fail $ "Can't find import file " ++ importFile
      where
        findFilePath dirs = fmap (</> importFile) <$> firstM (doesFileExist . (</> importFile)) dirs

    readFileUtf8 name = do
        h <- openFile name ReadMode
        hSetEncoding h utf8_bom
        hGetContents h


parseASTFile :: FilePath -> IO(Bond)
parseASTFile file = do
    input <- BL.readFile file
    case eitherDecode input of
        Left err -> do
            putStrLn $ "Error parsing " ++ file ++ ": " ++ show err
            exitFailure
        Right bond -> return bond


parseAliasMappings :: [String] -> IO [AliasMapping]
parseAliasMappings = mapM $
    \ s -> case parseAliasMapping s of
        Left err -> fail $ show err
        Right m -> return m


parseNamespaceMappings :: [String] -> IO [NamespaceMapping]
parseNamespaceMappings = mapM $
    \ s -> case parseNamespaceMapping s of
        Left err -> fail $ show err
        Right m -> return m

msbuildErrorMessage :: ParseError -> String
msbuildErrorMessage err = printf "%s(%d,%d) : error B0000: %s" name line col message
    where
        message = combinedMessage err
        pos = errorPos err
        name = sourceName pos
        line = sourceLine pos
        col = sourceColumn pos

combinedMessage :: ParseError -> String
combinedMessage err = id $ unpack $ intercalate (pack ", ") messages
    where
        -- showErrorMessages returns a multi-line String starting with a blank
        -- line. We need to break it up to make a useful one-line message.
        messages = splitOn (pack "\n") $ strip $ pack $
            showErrorMessages "or" "unknown parse error"
                "expecting" "unexpected" "end of input"
                (errorMessages err)
