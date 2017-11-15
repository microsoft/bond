-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Cpp.Types_cpp (types_cpp) where

import Data.Monoid
import Prelude
import Data.Text.Lazy (Text)
import Data.List
import Text.Shakespeare.Text
import Language.Bond.Syntax.Types
import Language.Bond.Syntax.Util
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.Util
import qualified Language.Bond.Codegen.Cpp.Util as CPP

-- | Codegen template for generating /base_name/_types.cpp containing
-- definitions of helper functions and schema metadata static variables.
types_cpp :: MappingContext -> String -> [Import] -> [Declaration] -> (String, Text)
types_cpp cpp file _imports declarations = ("_types.cpp", [lt|
#include "#{file}_reflection.h"
#include <bond/core/exception.h>

#{CPP.openNamespace cpp}
    #{doubleLineSepEnd 1 statics declarations}
#{CPP.closeNamespace cpp}
|])
  where
    -- definitions of Schema statics for non-generic structs
    statics s@Struct {..} =
        if null declParams then CPP.schemaMetadata cpp s else mempty

    -- global variables for enum name/value conversions
    --
    -- ToString is intentionally not implemented in terms of FromEnum, as
    -- ToString returns a reference to the name stored in the map. FromEnum
    -- copies this name into the output paramater.
    statics Enum {..} = [lt|
    namespace _bond_enumerators
    {
    namespace #{declName}
    {
        const std::map<std::string, enum #{declName}> _name_to_value_#{declName}
            {
                #{commaLineSep 4 nameValueConst $ enumConstByName}
            };

        const std::map<enum #{declName}, std::string> _value_to_name_#{declName}
            {
                #{commaLineSep 4 valueNameConst $ enumConstByValue}
            };

        const std::string& ToString(enum #{declName} value)
        {
            auto it = _value_to_name_#{declName}.find(value);

            if (_value_to_name_#{declName}.end() == it)
                ::bond::InvalidEnumValueException(value, "#{declName}");

            return it->second;
        }

        void FromString(const std::string& name, enum #{declName}& value)
        {
            if (!ToEnum(value, name))
                ::bond::InvalidEnumValueException(name.c_str(), "#{declName}");
        }

        bool ToEnum(enum #{declName}& value, const std::string& name)
        {
            auto it = _name_to_value_#{declName}.find(name);

            if (_name_to_value_#{declName}.end() == it)
                return false;

            value = it->second;

            return true;
        }

        bool FromEnum(std::string& name, enum #{declName} value)
        {
            auto it = _value_to_name_#{declName}.find(value);

            if (_value_to_name_#{declName}.end() == it)
                return false;

            name = it->second;

            return true;
        }

    } // namespace #{declName}
    } // namespace _bond_enumerators|]
      where
        nameValueConst Constant {..} = [lt|{ "#{constantName}", #{constantName} }|]
        valueNameConst (name, _) = [lt|{ #{name}, "#{name}" }|]
        enumConstByName = sortOn constantName enumConstants
        enumConstByValue = sortOn snd $ reifyEnumValues enumConstants

    statics _ = mempty
