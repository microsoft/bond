-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Bond.Template.Cpp.Enum_h (enum_h) where

import Data.Monoid
import Prelude
import Data.Text.Lazy (Text)
import Text.Shakespeare.Text
import Bond.Schema.Types
import Bond.Template.TypeMapping
import Bond.Template.Util
import qualified Bond.Template.Cpp.Util as CPP

-- generate the *_types.h file from parsed .bond file
enum_h :: MappingContext -> String -> [Import] -> [Declaration] -> (String, Text)
enum_h cpp _file _imports declarations = ("_enum.h", [lt|
#pragma once

#{CPP.openNamespace cpp}
namespace _bond_enumerators
{
    #{newlineSep 1 typeDeclaration declarations}
} // namespace _bond_enumerators

#{newlineSep 0 usingDeclaration declarations}
#{CPP.closeNamespace cpp}
|])
  where
    -- enum definition
    typeDeclaration e@Enum {..} = [lt|
    namespace #{declName}
    {
        #{CPP.enumDefinition e}
    } // namespace #{declName}
|]
    typeDeclaration _ = mempty

    usingDeclaration Enum {..} = [lt|using namespace _bond_enumerators::#{declName};|]
    usingDeclaration _ = mempty
