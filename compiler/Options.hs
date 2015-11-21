-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-cse #-}

module Options
    ( getOptions
    , processOptions
    , Options(..)
    , ApplyOptions(..)
    ) where

import Paths_bond (version)
import Data.Version (showVersion)
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit (processValue)

data ApplyOptions =
    Compact |
    Fast |
    Simple
    deriving (Show, Data, Typeable, Eq)

data Options
    = Options
    | Cpp
        { files :: [FilePath]
        , import_dir :: [FilePath]
        , output_dir :: FilePath
        , using :: [String]
        , namespace :: [String]
        , header :: [String]
        , enum_header :: Bool
        , allocator :: Maybe String
        , apply :: [ApplyOptions]
        , apply_attribute :: Maybe String
        , jobs :: Maybe Int
        , no_banner :: Bool
        }
    | Cs
        { files :: [FilePath]
        , import_dir :: [FilePath]
        , output_dir :: FilePath
        , using :: [String]
        , namespace :: [String]
        , collection_interfaces :: Bool
        , readonly_properties :: Bool
        , fields :: Bool
        , jobs :: Maybe Int
        , no_banner :: Bool
        }
    | Schema
        { files :: [FilePath]
        , import_dir :: [FilePath]
        , output_dir :: FilePath
        , jobs :: Maybe Int
        , runtime_schema :: Bool
        }
      deriving (Show, Data, Typeable)

cpp :: Options
cpp = Cpp
    { files = def &= typFile &= args
    , import_dir = def &= typDir &= name "i" &= help "Add the directory to import search path"
    , output_dir = "." &= typDir &= name "o" &= help "Output generated files into the specified directory"
    , using = def &= typ "MAPPING" &= name "u" &= help "Custom type alias mapping in the form alias=type"
    , namespace = def &= typ "MAPPING" &= name "n" &= help "Custom namespace mapping in the form bond_namespace=language_namespace"
    , header = def &= typ "HEADER" &= name "h" &= help "Emit #include HEADER into the generated files"
    , enum_header = def &= name "e" &= help "Generate enums into a separate header file"
    , allocator = def &= typ "ALLOCATOR" &= help "Generate types using the specified  allocator"
    , apply = def &= typ "PROTOCOL" &= help "Generate Apply function overloads for the specified protocol only; supported protocols: compact, fast and simple"
    , apply_attribute = def &= typ "ATTRIBUTE" &= help "Prefix the declarations of Apply functions with the specified C++ attribute/declspec"
    , jobs = def &= opt "0" &= typ "NUM" &= name "j" &= help "Run NUM jobs simultaneously (or '$ncpus' if no NUM is not given)"
    , no_banner = def &= help "Omit the banner at the top of generated files"
    } &=
    name "c++" &=
    help "Generate C++ code"

cs :: Options
cs = Cs
    { collection_interfaces = def &= name "c" &= help "Use interfaces rather than concrete collection types"
    , readonly_properties = def &= name "r" &= help "Generate private property setters"
    , fields = def &= name "f" &= help "Generate public fields rather than properties"
    } &=
    name "c#" &=
    help "Generate C# code"

schema :: Options
schema = Schema
    { runtime_schema = def &= help "Generate Simple JSON representation of runtime schema, aka SchemaDef"
    } &=
    name "schema" &=
    help "Output the JSON representation of the schema"


mode :: Mode (CmdArgs Options)
mode = cmdArgsMode $ modes [cpp, cs, schema] &=
    program "gbc" &=
    help "Compile Bond schema file(s) and generate specified output. The schema file(s) can be in one of two formats: Bond IDL or JSON representation of the schema abstract syntax tree as produced by `gbc schema`. Multiple schema files can be specified either directly on the command line or by listing them in a text file passed to gbc via @listfile syntax." &=
    summary ("Bond Compiler " ++ showVersion version ++ ", (C) Microsoft")

getOptions :: IO Options
getOptions = cmdArgsRun mode

processOptions :: [String] -> Options
processOptions = cmdArgsValue . processValue mode
