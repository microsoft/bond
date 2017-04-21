-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root
-- for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Java.Class_java
    ( class_java
    , JavaFieldMapping(..)
    ) where

import Prelude
import Data.Text.Lazy (Text)
import Text.Shakespeare.Text
import Language.Bond.Syntax.Types
import Language.Bond.Util
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.Util
import Language.Bond.Codegen.Java.Schema
import Language.Bond.Codegen.Java.SerializationMethods
import qualified Language.Bond.Codegen.Java.Util as Java

-- field -> public field
data JavaFieldMapping = JavaPublicFields deriving Eq

-- Template for struct -> Java class.
class_java :: MappingContext -> [Import] -> Declaration -> Text
class_java java _ declaration = [lt|
package #{javaPackage};

#{typeDefinition declaration}
|]
    where
        javaType = getTypeName java
        javaPackage = sepBy "." toText $ getNamespace java

        -- struct -> Java class
        typeDefinition Struct {..} = [lt|
#{Java.generatedClassAnnotations}
public class #{declName}#{params}#{maybe interface baseClass structBase} {
#{schemaMembers declaration}
    #{doubleLineSep 1 publicField structFields}
#{schemaStatic java declaration}
#{serialize_ProtocolWriter java declaration}
#{marshal_ProtocolWriter}
}|]
            where
                interface = [lt| implements com.microsoft.bond.BondSerializable|]
                params = angles $ sepBy ", " paramName declParams
                baseClass x = [lt| extends #{javaType x}()|]
                javaDefault = Java.defaultValue java

                -- FIXME: nullable<int32> -> Integer?
                publicField f@Field {..} = [lt|public #{javaType fieldType} #{fieldName} #{optional initializerValue $ javaDefault f};|]

                initializerValue x = [lt|= #{x}|]

        typeDefinition _ = mempty
