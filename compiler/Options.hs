-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-cse #-}
                                                     
module Options (getOptions, Options(..), ApplyOptions(..)) where

import Bond.Version
import System.Console.CmdArgs

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
        }
    | Haskell
        { files :: [FilePath]
        , import_dir :: [FilePath]
        , output_dir :: FilePath
        , using :: [String]
        , namespace :: [String]
        , jobs :: Maybe Int
        }
      deriving (Show, Data, Typeable)

cpp :: Options
cpp = Cpp 
    { files = def &= typFile &= args
    , import_dir = def &= typDir &= help "Add the directory to import search path"
    , output_dir = "." &= typDir &= help "Output generated files into the specified directory"
    , using = def &= typ "MAPPING" &= help "Custom type alias mapping in the form alias=type"
    , namespace = def &= typ "MAPPING" &= help "Custom namespace mapping in the from bond_namespace=language_namespace"
    , header = def &= typ "HEADER" &= help "Emit #include HEADER into the generated files"
    , enum_header = def &= help "Generate enums into a separate header file"
    , allocator = def &= typ "ALLOCATOR" &= help "Generate types using the specified  allocator"
    , apply = def &= typ "PROTOCOL" &= help "Generate Apply function overloads for the specified protocol only; supported protocols: compact, fast and simple"
    , apply_attribute = def &= typ "ATTRIBUTE" &= help "Prefix the declarations of Apply functions with the specified C++ attribute/declspec"
    , jobs = def &= opt "0" &= typ "NUM" &= help "Run NUM jobs simultaneously (or '$ncpus' if no NUM is not given)"
    } &= 
    name "c++" &=    
    help "Generate C++ code" 

cs :: Options
cs = Cs 
    { collection_interfaces = def &= help "Use interfaces rather than concrete collection types"
    , readonly_properties = def &= help "Generate private property setters"
    , fields = def &= help "Generate public fields rather than properties"
    } &= 
    name "c#" &= 
    help "Generate C# code"

haskell :: Options
haskell = Haskell { }
    &=
    name "haskell" &=
    help "Generate Haskell code"

mode :: Mode (CmdArgs Options)
mode = cmdArgsMode $ modes [cpp, cs, haskell] &=
    program "gbc" &= 
    help "Compile Bond schema definition file and generate specified output" &=
    summary ("Bond Compiler " ++ majorVersion ++ "." ++ minorVersion ++ ", (C) Microsoft")
                     
getOptions :: IO Options
getOptions = cmdArgsRun mode
