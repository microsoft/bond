-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

import System.Exit
import System.Environment (getArgs)
import System.Directory
import System.FilePath
import System.IO
import Data.Monoid
import Control.Monad
import Prelude
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as L
import Language.Bond.Parser
import Language.Bond.Syntax.Types (Bond(..), Declaration(..), Import)
import Language.Bond.Codegen.Util
import Language.Bond.Codegen.Templates
import Language.Bond.Codegen.TypeMapping
import Ctor_cs

-- This is a very simple example of custom codegen implemented using the "bond"
-- package. For code clarity the example omits error handling and doesn't
-- implement some features (e.g. import directories).
--
-- The example demonstrates how to parse .bond files and use the standard
-- templates with custom options as well as how to write a custom codegen
-- template.
--
-- The program built from this example compiles Bond schema file(s) passed as
-- command line arguments and generates C# class(s) with read-only properties.
-- It also generates an additional file with '_ctor.cs' suffix, adding
-- a definition of a constructor taking an argument for each schema field.

type Template = MappingContext -> String -> [Import] -> [Declaration] -> (String, Text)

main :: IO()
main = do
    files <- getArgs
    forM_ files $ codeGen 
        [ types_cs Class ReadOnlyProperties 
        , ctor_cs
        ]

parseBondFile :: FilePath -> IO(Bond)
parseBondFile file = do
    cwd <- getCurrentDirectory
    input <- readFile file
    result <- parseBond file input (cwd </> file) readImportFile
    case result of
        Left err -> do
            putStrLn $ "Error parsing " ++ file ++ ": " ++ show err
            exitFailure
        Right bond -> return bond
  where
    readImportFile parentFile importFile = do
        let path = (takeDirectory parentFile) </> importFile
        content <- readFile path
        return (path, content)

codeGen :: [Template] -> FilePath -> IO ()
codeGen templates file = do
    let baseName = takeBaseName file
    (Bond imports namespaces declarations) <- parseBondFile file
    let mappingContext = MappingContext csTypeMapping [] [] namespaces
    forM_ templates $ \template -> do
        let (suffix, code) = template mappingContext baseName imports declarations
        L.writeFile (baseName ++ suffix) code
