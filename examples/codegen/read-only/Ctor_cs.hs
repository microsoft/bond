-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Ctor_cs
    ( ctor_cs
    ) where

import Data.Monoid
import Prelude
import Data.Text.Lazy (Text)
import Text.Shakespeare.Text
import Language.Bond.Syntax.Types
import Language.Bond.Util
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.Util

ctor_cs :: MappingContext -> String -> [Import] -> [Declaration] -> (String, Text)
ctor_cs cs _ _ declarations = ("_ctor.cs", [lt|
namespace #{csNamespace}
{
    using System.Collections.Generic;

    #{doubleLineSep 1 ctor declarations}
}
|])
  where
    csNamespace = sepBy "." toText $ getNamespace cs

    ctor Struct {..} = [lt|public partial class #{declName}
    {
        public #{declName}(#{commaLineSep 3 parameter structFields})
        {
            #{newlineSep 3 initializer structFields}
        }
    }|]
      where
        parameter Field {..} = [lt|#{getTypeName cs fieldType} #{fieldName}|]
        initializer Field {..} = [lt|this.#{fieldName} = #{fieldName};|]

    ctor _ = mempty
