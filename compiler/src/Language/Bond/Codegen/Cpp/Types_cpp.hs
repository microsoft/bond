-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Cpp.Types_cpp (types_cpp) where

import Data.Monoid
import Prelude
import Data.Text.Lazy (Text)
import Text.Shakespeare.Text
import Language.Bond.Syntax.Types
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.Util
import qualified Language.Bond.Codegen.Cpp.Util as CPP

-- | Codegen template for generating /base_name/_types.cpp containing
-- definitions of helper functions and schema metadata static variables.
types_cpp :: MappingContext -> String -> [Import] -> [Declaration] -> (String, Text)
types_cpp cpp file _imports declarations = ("_types.cpp", [lt|
#include "#{file}_reflection.h"
#include <bond/core/exception.h>
#{unorderedMapInclude}
#{CPP.openNamespace cpp}
    #{doubleLineSepEnd 1 statics declarations}
#{CPP.closeNamespace cpp}
|])
  where
    unorderedMapInclude = if not (any CPP.isEnumDeclaration declarations) then mempty else [lt|#include <unordered_map>
|]

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
        namespace
        {
            struct _hash_#{declName}
            {
                std::size_t operator()(enum #{declName} value) const
                {
                    return static_cast<std::size_t>(value);
                }
            };
        }
        const std::string& ToString(enum #{declName} value)
        {
            const auto& map = GetValueToNameMap<std::unordered_map<enum #{declName}, std::string, _hash_#{declName}> >(value);
            auto it = map.find(value);

            if (map.end() == it)
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
            const auto& map = GetNameToValueMap<std::unordered_map<std::string, enum #{declName}> >(value);
            auto it = map.find(name);

            if (map.end() == it)
                return false;

            value = it->second;

            return true;
        }

        bool FromEnum(std::string& name, enum #{declName} value)
        {
            const auto& map = GetValueToNameMap<std::unordered_map<enum #{declName}, std::string, _hash_#{declName}> >(value);
            auto it = map.find(value);

            if (map.end() == it)
                return false;

            name = it->second;

            return true;
        }

    } // namespace #{declName}
    } // namespace _bond_enumerators|]

    statics _ = mempty
