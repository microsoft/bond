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
import qualified Language.Bond.Codegen.Cpp.Util as CPP

-- | Codegen template for generating /base_name/_apply.h containing declarations of
-- <https://microsoft.github.io/bond/manual/bond_cpp.html#optimizing-build-time Apply>
-- function overloads for the specified protocols.
apply_h :: [Protocol]   -- ^ List of protocols for which @Apply@ overloads should be generated
        -> Maybe String -- ^ Optional attribute to decorate the @Apply@ function declarations
        -> MappingContext -> String -> [Import] -> [Declaration] -> (String, Text)
apply_h protocols attribute cpp file imports declarations = ("_apply.h", [lt|
#pragma once

#include "#{file}_types.h"
#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>
#{newlineSep 0 includeImport imports}

#{CPP.openNamespace cpp}
    #{newlineSepEnd 1 (applyOverloads protocols attr semi) declarations}
#{CPP.closeNamespace cpp}
|])
  where
    includeImport (Import path) = [lt|#include "#{dropExtension path}_apply.h"|]

    attr = optional (\a -> [lt|#{a}
    |]) attribute

    semi = [lt|;|]

