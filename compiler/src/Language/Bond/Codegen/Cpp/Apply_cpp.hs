-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Language.Bond.Codegen.Cpp.Apply_cpp (apply_cpp) where

import Data.Text.Lazy (Text)
import Text.Shakespeare.Text
import Language.Bond.Syntax.Types
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.Util
import Language.Bond.Codegen.Cpp.ApplyOverloads

-- | Codegen template for generating /base_name/_apply.cpp containing
-- definitions of the @Apply@ function overloads for the specified protocols.
apply_cpp :: [Protocol]   -- ^ List of protocols for which @Apply@ overloads should be generated
          -> MappingContext -> String -> [Import] -> [Declaration] -> (String, Text)
apply_cpp protocols cpp file _imports declarations = ("_apply.cpp", [lt|
#include "#{file}_apply.h"
#include "#{file}_reflection.h"

namespace bond
{
    #{newlineSepEnd 1 (applyOverloads protocols cpp attr extern) declarations}
} // namespace bond
|])
  where
    attr = ""
    extern = ""

