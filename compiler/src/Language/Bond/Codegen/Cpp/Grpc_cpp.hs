-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Cpp.Grpc_cpp (grpc_cpp) where

import Data.Monoid
import Prelude
import Data.Text.Lazy (Text)
import Text.Shakespeare.Text
import Language.Bond.Syntax.Types
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.Util
import qualified Language.Bond.Codegen.Cpp.Util as CPP

-- | Codegen template for generating /base_name/_grpc.cpp containing
-- definitions of helper functions and schema metadata static variables.
grpc_cpp :: MappingContext -> String -> [Import] -> [Declaration] -> (String, Text)
grpc_cpp cpp file _imports declarations = ("_grpc.cpp", [lt|
#include "#{file}_reflection.h"
#include "#{file}_grpc.h"

#{CPP.openNamespace cpp}
    #{doubleLineSepEnd 1 statics declarations}
#{CPP.closeNamespace cpp}
|])
  where
    -- definitions of Schema statics for non-generic services
    statics s@Service {..} =
        if null declParams then CPP.schemaMetadata cpp s else mempty

    statics _ = mempty
