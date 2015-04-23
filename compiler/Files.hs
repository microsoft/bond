-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

module Files
    ( parseFile
    , parseBondFile
    , parseASTFile
    )
    where

import System.Exit
import System.FilePath
import System.Directory
import System.IO
import Control.Applicative
import Prelude
import Data.Aeson (eitherDecode)
import Control.Monad.Loops (firstM)
import qualified Data.ByteString.Lazy as BL
import Bond.Schema.Types (Bond(..))
import Bond.Schema.JSON()
import Bond.Parser


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
            print $ "Error parsing " ++ file ++ ": " ++ show err
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
            print $ "Error parsing " ++ file ++ ": " ++ show err
            exitFailure
        Right bond -> return bond

