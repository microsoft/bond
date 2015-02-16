-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Bond.Template.Cpp.Apply_cpp (apply_cpp) where

import Data.String (IsString)
import Data.Text.Lazy (Text)
import Text.Shakespeare.Text
import Bond.Schema
import Bond.Template.TypeMapping
import Bond.Template.Util
import Bond.Template.Cpp.Apply_h
import qualified Bond.Template.Cpp.Util as CPP

-- generate the *_apply.cpp file from parsed .bond file
apply_cpp :: (ToText a, IsString t1)
          => [Protocol]
          -> Context
          -> a
          -> t
          -> [Declaration]
          -> (t1, Text)
apply_cpp protocols cpp file _imports declarations = ("_apply.cpp", [lt|
#include "#{file}_apply.h"
#include "#{file}_reflection.h"

#{CPP.openNamespace cpp}
    #{newlineSepEnd 1 (apply protocols attr body) declarations}
#{CPP.closeNamespace cpp}
|])
  where
    body = [lt|
    {
        return bond::Apply<>(transform, value);
    }|]

    attr = [lt||]

