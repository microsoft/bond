-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-|
Copyright   : (c) Microsoft
License     : MIT
Maintainer  : adamsap@microsoft.com
Stability   : provisional
Portability : portable

The module exports the built-in code generation templates.
-}

module Language.Bond.Codegen.Templates
    ( -- * Templates
      -- | All codegen templates take at least the following arguments:
      --
      -- * 'MappingContext' which determines mapping of Bond types to types in
      --   the target language
      --
      -- * base file name, typically the name of the schema definition file
      --   without the extension
      --
      -- * list of 'Import's from the parsed schema definition
      --
      -- * list of 'Declaration's from the parsed schema definition
      --
      -- Some templates are parameterized with additional options for
      -- customizing the generated code.
      --
      -- The templates return the name suffix for the target file and lazy
      -- 'Text' with the generated code.

      -- ** C++
      types_h
    , types_cpp
    , reflection_h
    , enum_h
    , apply_h
    , apply_cpp
    ,  Protocol(..)
      -- ** C#
    , FieldMapping(..)
    , StructMapping(..)
    , ConstructorOptions(..)
    , types_cs
      -- ** Java
    , JavaFieldMapping(..)
    , class_java
    , enum_java
    )
    where

import Language.Bond.Codegen.Cpp.Apply_cpp
import Language.Bond.Codegen.Cpp.Apply_h
import Language.Bond.Codegen.Cpp.ApplyOverloads
import Language.Bond.Codegen.Cpp.Enum_h
import Language.Bond.Codegen.Cpp.Reflection_h
import Language.Bond.Codegen.Cpp.Types_cpp
import Language.Bond.Codegen.Cpp.Types_h
import Language.Bond.Codegen.Cs.Types_cs
import Language.Bond.Codegen.Java.Class_java
import Language.Bond.Codegen.Java.Enum_java
-- redundant imports for haddock
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Syntax.Types
import Data.Text.Lazy
