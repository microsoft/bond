-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Cs.Types_cs
    ( types_cs
    , FieldMapping(..)
    , StructMapping(..)
    ) where

import Data.Monoid
import qualified Data.Foldable as F
import Prelude
import Data.Text.Lazy (Text)
import Text.Shakespeare.Text
import Language.Bond.Syntax.Types
import Language.Bond.Syntax.Util
import Language.Bond.Syntax.Internal
import Language.Bond.Util
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.Util
import qualified Language.Bond.Codegen.Cs.Util as CS

-- | C# representation of schema structs
data StructMapping =
    Class                   -- ^ public partial class
    deriving Eq

-- | Representation of schema fields in the generated C# types
data FieldMapping =
    PublicFields |          -- ^ public fields
    Properties |            -- ^ auto-properties
    ReadOnlyProperties      -- ^ auto-properties with private setter
    deriving Eq

-- | Codegen template for generating definitions of C# types representing the schema.
types_cs
    :: StructMapping        -- ^ Specifies how to represent schema structs
    -> FieldMapping         -- ^ Specifies how to represent schema fields
    -> MappingContext -> String -> [Import] -> [Declaration] -> (String, Text)
types_cs structMapping fieldMapping cs _ _ declarations = (fileSuffix, [lt|
#{CS.disableCscWarnings}
#{CS.disableReSharperWarnings}
namespace #{csNamespace}
{
    using System.Collections.Generic;

    #{doubleLineSep 1 typeDefinition declarations}
} // #{csNamespace}
|])
  where
    idl = MappingContext idlTypeMapping [] [] []  

    -- C# type
    csType = getTypeName cs
    csNamespace = sepBy "." toText $ getNamespace cs

    access = case structMapping of
        _ -> [lt|public |]

    fileSuffix = case structMapping of
        _ -> "_types.cs"

    struct = case structMapping of
        _ -> [lt|public partial class |]

    typeAttributes s = case structMapping of
        _ -> CS.typeAttributes cs s

    propertyAttributes f = case structMapping of
        Class -> CS.propertyAttributes cs f

    -- C# type definition for schema struct
    typeDefinition s@Struct {..} = [lt|#{typeAttributes s}#{struct}#{declName}#{params}#{maybe interface baseClass structBase}#{constraints}
    {
        #{doubleLineSep 2 property structFields}#{constructors}
    }|]
      where
        interface = case structMapping of
            _ -> mempty

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
        constructors = if noCtor then mempty else [lt|

        public #{declName}()
            : this("#{getDeclTypeName idl s}", "#{declName}")
        {}

        protected #{declName}(string fullName, string name)#{baseCtor}
        {
            #{newlineSep 3 initializer structFields}
        }|]
          where
            noCtor = not callBaseCtor && (fieldMapping == PublicFields && noMetaFields || null structFields)
            noMetaFields = not $ getAny $ F.foldMap metaField structFields

        -- property or field
        property f@Field {..} =
            [lt|#{propertyAttributes f}#{new}#{access}#{csType fieldType} #{fieldName}#{autoPropertyOrField}|]
          where
            autoPropertyOrField = case fieldMapping of
                PublicFields        -> [lt|#{optional fieldInitializer $ csDefault f};|]
                Properties          -> [lt| { get; set; }|]
                ReadOnlyProperties  -> [lt| { get; private set; }|]
            fieldInitializer x = [lt| = #{x}|]
            new = if isBaseField fieldName structBase then "new " else "" :: String

        -- initializers in constructor
        initializer f@Field {..} = optional fieldInit $ def f
          where
            fieldInit x = [lt|#{this fieldName} = #{x};|]
            this = if fieldName == "name" || fieldName == "fullName" then ("this." ++) else id
            def Field {fieldType = BT_MetaName} = Just "name"
            def Field {fieldType = BT_MetaFullName} = Just "fullName"
            def x = if fieldMapping == PublicFields then Nothing else csDefault x

    -- C# enum definition for schema enum
    typeDefinition e@Enum {..} = [lt|#{CS.typeAttributes cs e}public enum #{declName}
    {
        #{newlineSep 2 constant enumConstants}
    }|]
      where
        -- constant
        constant Constant {..} = let value x = [lt| = unchecked((int)#{x})|] in
            [lt|#{constantName}#{optional value constantValue},|]

    typeDefinition _ = mempty
