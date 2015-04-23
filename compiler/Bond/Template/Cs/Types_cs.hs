-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Bond.Template.Cs.Types_cs (types_cs) where

import Data.Monoid
import qualified Data.Foldable as F
import Prelude
import Data.Text.Lazy (Text)
import Text.Shakespeare.Text
import Bond.Schema.Types
import Bond.Schema.Util
import Bond.Util
import Bond.Template.TypeMapping
import Bond.Template.Util
import qualified Bond.Template.Cs.Util as CS

-- generate the *_types.cs file from parsed .bond file
types_cs :: Bool -> Bool -> MappingContext -> String -> [Import] -> [Declaration] -> (String, Text)
types_cs readOnly useFields cs _file _imports declarations = ("_types.cs", [lt|
#{CS.disableReSharperWarnings}
namespace #{csNamespace}
{
    using System.Collections.Generic;

    #{doubleLineSep 1 typeDefinition declarations}
} // #{csNamespace}
|])
  where
    idlNamespace = getIdlQualifiedName $ getIdlNamespace cs

    -- C# type
    csType = getTypeName cs
    csNamespace = getQualifiedName csTypeMapping $ getNamespace cs

    -- C# class definition for schema struct
    typeDefinition s@Struct {..} = [lt|#{CS.typeAttributes cs s}
    public partial class #{declName}#{params}#{optional baseClass structBase}#{constraints}
    {
        #{doubleLineSep 2 property structFields}
        #{constructors}
    }|]
      where
        -- type parameters
        params = angles $ sepBy ", " paramName declParams

        -- constraints
        constraints = CS.paramConstraints declParams

        -- base
        callBaseCtor = getAny $ optional (foldMapFields metaField) structBase

        baseClass x = [lt|
        : #{csType x}|]

        baseCtor = if not callBaseCtor then mempty else [lt|
            : base(fullName, name)|]

        -- default value
        csDefault = CS.defaultValue cs

        -- constructors
        constructors = if emptyCtor then mempty else [lt|
        public #{declName}()
            : this("#{idlNamespace}.#{declName}", "#{declName}")
        {}

        protected #{declName}(string fullName, string name)#{baseCtor}
        {
            #{newlineSep 3 initializer structFields}
        }|]
          where
            emptyCtor = not callBaseCtor && (useFields && noMetaFields || null structFields)
            noMetaFields = not $ getAny $ F.foldMap metaField structFields

        -- property or field
        property f@Field {..} = [lt|#{CS.propertyAttributes cs f}
        #{new}public #{csType fieldType} #{fieldName}#{autoPropertyOrField}|]
          where
            autoPropertyOrField =
                if useFields then
                    [lt|#{optional fieldInitializer $ csDefault f};|]
                else
                    [lt| { get; #{set}; }|]
            fieldInitializer x = [lt| = #{x}|]
            set = if readOnly then "private set" else "set" :: String
            new = if isBaseField fieldName structBase then "new " else "" :: String

        -- initializers in constructor
        initializer f@Field {..} = optional fieldInit $ def f
          where 
            fieldInit x = [lt|#{this fieldName} = #{x};|]
            this = if fieldName == "name" || fieldName == "fullName" then ("this." ++) else id
            def Field {fieldType = BT_MetaName} = Just "name"
            def Field {fieldType = BT_MetaFullName} = Just "fullName"
            def x = if useFields then Nothing else csDefault x

    -- C# enum definition for schema enum
    typeDefinition e@Enum {..} = [lt|
    #{CS.typeAttributes cs e}
    public enum #{declName}
    {
        #{newlineSep 2 constant enumConstants}
    }|]
      where
        -- constant
        constant Constant {..} = let value x = [lt| = #{x}|] in
            [lt|#{constantName}#{optional value constantValue},|]

    typeDefinition _ = mempty
