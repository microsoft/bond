-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
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
import IO (slashNormalize)

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
        , export_attribute :: Maybe String
        , jobs :: Maybe Int
        , no_banner :: Bool
        , core_enabled :: Bool
        , alloc_ctors_enabled :: Bool
        , type_aliases_enabled :: Bool
        , scoped_alloc_enabled :: Bool
        , service_inheritance_enabled :: Bool
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
        , structs_enabled :: Bool
        , service_inheritance_enabled :: Bool
        , constructor_parameters :: Bool
        }
    | Java
        { files :: [FilePath]
        , import_dir :: [FilePath]
        , output_dir :: FilePath
        , using :: [String]
        , namespace :: [String]
        , jobs :: Maybe Int
        , no_banner :: Bool
        }
    | Schema
        { files :: [FilePath]
        , import_dir :: [FilePath]
        , output_dir :: FilePath
        , jobs :: Maybe Int
        , runtime_schema :: Bool
        , service_inheritance_enabled :: Bool
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
    , export_attribute = def &= typ "ATTRIBUTE" &= explicit &= name "apply-attribute" &= name "export-attribute" &= help "Prefix declarations for library export with the specified C++ attribute/declspec. apply-attribute is a deprecated synonym."
    , jobs = def &= opt "0" &= typ "NUM" &= name "j" &= help "Run NUM jobs simultaneously (or '$ncpus' if no NUM is not given)"
    , no_banner = def &= help "Omit the banner at the top of generated files"
    , core_enabled = True &= explicit &= name "core" &= help "Generate core serialization definitions (true by default, --core=false to disable)"
    , alloc_ctors_enabled = False &= explicit &= name "alloc-ctors" &= help "Generate constructors with allocator argument"
    , type_aliases_enabled = False &= explicit &= name "type-aliases" &= help "Generate type aliases"
    , scoped_alloc_enabled = False &= explicit &= name "scoped-alloc" &= help "Use std::scoped_allocator_adaptor for strings and containers"
    , service_inheritance_enabled = False &= explicit &= name "enable-service-inheritance" &= help "Enable service inheritance syntax in IDL"
    } &=
    name "c++" &=
    help "Generate C++ code"

cs :: Options
cs = Cs
    { collection_interfaces = def &= name "c" &= help "Use interfaces rather than concrete collection types"
    , readonly_properties = def &= name "r" &= help "Generate private property setters"
    , fields = def &= name "f" &= help "Generate public fields rather than properties"
    , structs_enabled = True &= explicit &= name "structs" &= help "Generate C# types for Bond structs and enums (true by default, use \"--structs=false\" to disable)"
    , constructor_parameters = def &= explicit &= name "preview-constructor-parameters" &= help "PREVIEW FEATURE: Generate a constructor that takes all the fields as parameters. Typically used with readonly-properties."
    } &=
    name "c#" &=
    help "Generate C# code"

java :: Options
java = Java
    { using = def &= typ "MAPPING" &= name "u" &= help "Currently unimplemented and ignored for Java"
    } &=
    name "java" &=
    help "Generate Java code"

schema :: Options
schema = Schema
    { runtime_schema = def &= help "Generate Simple JSON representation of runtime schema, aka SchemaDef"
    } &=
    name "schema" &=
    help "Output the JSON representation of the schema"

slashNormalizeOption :: Options -> Options
slashNormalizeOption Options = Options
slashNormalizeOption o@Cpp{..}    = o { files = map slashNormalize files,
                                        import_dir = map slashNormalize import_dir,
                                        output_dir = slashNormalize output_dir }
slashNormalizeOption o@Cs{..}     = o { files = map slashNormalize files,
                                        import_dir = map slashNormalize import_dir,
                                        output_dir = slashNormalize output_dir }
slashNormalizeOption o@Java{..}   = o { files = map slashNormalize files,
                                        import_dir = map slashNormalize import_dir,
                                        output_dir = slashNormalize output_dir }
slashNormalizeOption o@Schema{..} = o { files = map slashNormalize files,
                                        import_dir = map slashNormalize import_dir,
                                        output_dir = slashNormalize output_dir }
                                   

mode :: Mode (CmdArgs Options)
mode = cmdArgsMode $ modes [cpp, cs, java, schema] &=
    program "gbc" &=
    help "Compile Bond schema file(s) and generate specified output. The schema file(s) can be in one of two formats: Bond IDL or JSON representation of the schema abstract syntax tree as produced by `gbc schema`. Multiple schema files can be specified either directly on the command line or by listing them in a text file passed to gbc via @listfile syntax." &=
    summary ("Bond Compiler " ++ showVersion version ++ ", (C) Microsoft")

getOptions :: IO Options
getOptions = slashNormalizeOption <$> cmdArgsRun mode

processOptions :: [String] -> Options
processOptions = cmdArgsValue . processValue mode
