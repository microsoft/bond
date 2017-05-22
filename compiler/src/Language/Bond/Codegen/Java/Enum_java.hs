-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root
-- for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Java.Enum_java
    ( enum_java
    ) where

import Prelude
import Data.Monoid
import Data.Text.Lazy (Text)
import Text.Shakespeare.Text
import Language.Bond.Syntax.Types
import Language.Bond.Util
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.Util
import qualified Language.Bond.Codegen.Java.Util as Java

-- Template for enum -> Java enum-like class.
enum_java :: MappingContext -> Declaration -> Text
enum_java java declaration = [lt|
package #{javaPackage};

#{typeDefinition declaration}
|]
  where
    javaPackage = sepBy "." toText $ getNamespace java

    typeDefinition Enum {..} = [lt|
#{Java.generatedClassAnnotations}
public class #{declName} {
    #{newlineSep 1 constant enumConstantsWithInt}

    public final int value;

    public #{declName}(int value) { this.value = value; }

    @Override
    public boolean equals(Object other) {
        if (!(other instanceof #{declName})) { return false; }
        return this.value == ((#{declName}) other).value;
    }

    @Override
    public int hashCode() {
        return this.value;
    }
}|]
      where
        -- constant
        constant Constant {..} = let value x = [lt|#{x}|] in
            [lt|public static final #{declName} #{constantName} = new #{declName}(#{optional value constantValue});|]

        -- Process constants to make sure all values can be converted
        -- to integer.
        enumConstantsWithInt = fixEnumWithInt 0 enumConstants []

        fixEnumWithInt :: Int -> [Constant] -> [Constant] -> [Constant]
        fixEnumWithInt _ [] result = reverse result
        fixEnumWithInt nextInt (h:r) result = case constantValue h of
          Just n -> fixEnumWithInt (n + 1) r (h:result)
          _ -> fixEnumWithInt (nextInt + 1) r ((Constant (constantName h) (Just nextInt)):result)

    typeDefinition _ = mempty
