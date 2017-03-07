-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root
-- for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Java.Class_java
    ( class_java
    , JavaFieldMapping(..)
    ) where

import Data.Monoid
import Prelude
import Data.Text.Lazy (Text)
import Text.Shakespeare.Text
import Language.Bond.Syntax.Types
import Language.Bond.Util
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.Util
import qualified Language.Bond.Codegen.Java.Util as Java

-- field -> public field
data JavaFieldMapping = JavaPublicFields deriving Eq

-- Template for struct -> Java class and enum -> Java enum.
class_java :: MappingContext -> [Import] -> Declaration -> Text
class_java java _ declaration = [lt|
package #{javaPackage};

// Standard imports used by Bond.
import java.math.BigInteger;
import java.util.*;

// Bond lib imports.
import com.microsoft.bond.*;

// Imports for other generated code.
import #{javaPackage}.*;

#{typeDefinition declaration}
|]
  where
    javaType = getTypeName java
    javaPackage = sepBy "." toText $ getNamespace java
    struct = [lt|public class|]



    -- struct -> Java class
    typeDefinition Struct {..} = [lt|
#{struct} #{declName}#{params}#{maybe interface baseClass structBase} {
    #{doubleLineSep 1 publicField structFields}
}|]
      where
        interface = [lt| implements BondSerializable|]
        params = angles $ sepBy ", " paramName declParams
        baseClass x = [lt| extends #{javaType x}()|]
        javaDefault = Java.defaultValue java

        -- FIXME: nullable<int32> -> Integer?
        publicField f@Field {..} = [lt|public #{javaType fieldType} #{fieldName} #{optional initializerValue $ javaDefault f};|]

        initializerValue x = [lt|= #{x}|]



    -- enum -> Java enum
    typeDefinition Enum {..} = [lt|
public enum #{declName} {
    #{commaLineSep 1 constant enumConstantsWithInt};

    private int value;

    private #{declName}(int value) { this.value = value; }
}|]
      where
        -- constant
        constant Constant {..} = let value x = [lt|(#{x})|] in
            [lt|#{constantName}#{optional value constantValue}|]

        -- Process constants to make sure all values can be converted
        -- to integer.
        enumConstantsWithInt = fixEnumWithInt 0 enumConstants []

        fixEnumWithInt :: Int -> [Constant] -> [Constant] -> [Constant]
        fixEnumWithInt _ [] result = reverse result
        fixEnumWithInt nextInt (h:r) result = case constantValue h of
          Just n -> fixEnumWithInt (n + 1) r (h:result)
          _ -> fixEnumWithInt (nextInt + 1) r ((Constant (constantName h) (Just nextInt)):result)



    -- Bond _ -> nothing
    typeDefinition _ = mempty
