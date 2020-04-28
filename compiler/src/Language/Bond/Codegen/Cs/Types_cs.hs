-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Cs.Types_cs
    ( types_cs
    , FieldMapping(..)
    , StructMapping(..)
    , ConstructorOptions(..)
    ) where

import Data.Monoid
import Prelude
import Data.Text.Lazy (Text, pack)
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

-- | Options for how constructors should be generated.
data ConstructorOptions =
    DefaultWithProtectedBase | -- ^ The original bond behavior.
    ConstructorParameters      -- ^ Generate a constructor that takes all the fields as parameters.
    deriving Eq

-- | Codegen template for generating definitions of C# types representing the schema.
types_cs
    :: StructMapping        -- ^ Specifies how to represent schema structs
    -> FieldMapping         -- ^ Specifies how to represent schema fields
    -> ConstructorOptions   -- ^ Specifies the constructors that should be generated
    -> MappingContext -> String -> [Import] -> [Declaration] -> (String, Text)
types_cs structMapping fieldMapping constructorOptions cs _ _ declarations = (fileSuffix, [lt|
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

    baseClass x = [lt|
        : #{csType x}|]

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

        -- default value
        csDefault = CS.defaultValue cs

        metaFields = filter (isMetaName . fieldType) structFields

        noMetaFields = null metaFields

        -- constructor: DefaultWithProtectedBase option
        defaultWithProtectedBaseConstructor = if noCtor then mempty else [lt|

        public #{declName}()
            : this("#{getDeclTypeName idl s}", "#{declName}")
        {}

        protected #{declName}(string fullName, string name)#{baseCtor}
        {
            #{newlineSep 3 initializer structFields}
        }|]
          where
            noCtor = not callBaseCtor && (fieldMapping == PublicFields && noMetaFields || null structFields)
            callBaseCtor = getAny $ optional (foldMapFields metaField) structBase
            baseCtor = if not callBaseCtor
                then mempty
                else [lt|
            : base(fullName, name)|]

        -- constructor: ConstructorParameters option
        constructorWithParameters = if not noMetaFields
            then error $ "bond_meta usage in Struct " ++ (show declName) ++ " Field " ++ (show $ fieldName $ head metaFields) ++ " is incompatible with --preview--constructor-parameters"
            else if (null baseFieldList)
                then if (null structFields) 
                     then [lt|
        #{defaultConstructor}|]
                     else [lt|

        public #{declName}(
            #{commaLineSep 3 paramDecl fieldNameList})
        {
            #{newlineSep 3 paramBasedInitializer fieldNameList}
        }

        #{defaultConstructor}|]
                else [lt|

        public #{declName}(
            // Base class parameters
            #{commaLineSep 3 paramDecl (zip baseFieldList uniqueBaseFieldNames)}#{thisParamBlock}
        ) : base(
                #{commaLineSep 4 pack uniqueBaseFieldNames})
        {
            #{newlineSep 3 paramBasedInitializer (zip structFields uniqueThisFieldNames)}
        }

        #{defaultConstructor}|]

        thisParamBlock = if null structFields
            then mempty
            else [lt|,

            // This class parameters
            #{commaLineSep 3 paramDecl (zip structFields uniqueThisFieldNames)}|]

        defaultConstructor = [lt|public #{declName}()
        {
            #{newlineSep 3 initializer structFields}
        }|]

        baseFieldList = concat $ baseFields s

        uniqueBaseFieldNames = uniqueNames (map fieldName baseFieldList) []
        uniqueThisFieldNames = uniqueNames (map fieldName structFields) uniqueBaseFieldNames

        paramDecl (f, n) = [lt|#{csType $ fieldType f} #{n}|]

        paramBasedInitializer (f, n) = [lt|this.#{fieldName f} = #{n};|]

        fieldNameList = map (\f -> (f, fieldName f)) structFields

        constructors = case constructorOptions of
            DefaultWithProtectedBase -> defaultWithProtectedBaseConstructor
            ConstructorParameters -> constructorWithParameters

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
