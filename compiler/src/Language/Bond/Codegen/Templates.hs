-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-|
Copyright   : (c) Microsoft
License     : MIT
Maintainer  : adamsap@microsoft.com
Stability   : alpha
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

      -- ** Protocol data type
      Protocol(..)
      -- ** C++
    , types_h
    , types_cpp
    , reflection_h
    , enum_h
    , apply_h
    , apply_cpp
      -- ** C#
    , FieldMapping(..)
    , StructMapping(..)
    , types_cs
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

