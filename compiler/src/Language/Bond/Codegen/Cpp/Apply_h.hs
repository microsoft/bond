-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Cpp.Apply_h (apply_h) where

import System.FilePath
import Prelude
import Data.Text.Lazy (Text)
import Text.Shakespeare.Text
import Language.Bond.Syntax.Types
import Language.Bond.Util
import Language.Bond.Codegen.Util
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.Cpp.ApplyOverloads

-- | Codegen template for generating /base_name/_apply.h containing declarations of
-- <https://microsoft.github.io/bond/manual/bond_cpp.html#optimizing-build-time Apply>
-- function overloads for the specified protocols.
apply_h :: [Protocol]   -- ^ List of protocols for which @Apply@ overloads should be generated
        -> Maybe String -- ^ Optional attribute to decorate the @Apply@ function declarations
        -> MappingContext -> String -> [Import] -> [Declaration] -> (String, Text)
apply_h protocols export_attribute cpp file imports declarations = ("_apply.h", [lt|
#pragma once

#include "#{file}_types.h"
#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>
#{newlineSep 0 includeImport imports}

namespace bond
{
    #{newlineSepEnd 1 (applyOverloads protocols cpp export_attr extern) declarations}
} // namespace bond
|])
  where
    includeImport (Import path) = [lt|#include "#{dropExtension (slashForward path)}_apply.h"|]

    export_attr = optional (\a -> [lt|#{a}|]) export_attribute

    extern = "extern "

