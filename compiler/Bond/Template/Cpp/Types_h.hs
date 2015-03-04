-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Bond.Template.Cpp.Types_h (types_h) where

import System.FilePath
import Data.Maybe
import Data.List
import Data.Monoid
import Data.Text.Lazy.Builder
import qualified Data.Text.Lazy as L
import Data.Foldable (foldMap)
import Text.Shakespeare.Text
import Bond.Version
import Bond.Schema
import Bond.Util
import Bond.Template.TypeMapping
import Bond.Template.Util
import qualified Bond.Template.Cpp.Util as CPP

-- generate the *_types.h file from parsed .bond file
types_h :: [String]
        -> Bool
        -> Maybe String
        -> MappingContext -> String -> [Import] -> [Declaration] -> (String, L.Text)
types_h userHeaders enumHeader allocator cpp file imports declarations = ("_types.h", [lt|
#pragma once
#{newlineBeginSep 0 includeHeader userHeaders}
#include <bond/core/bond_version.h>

#if BOND_VERSION < 0x302
#error This file was generated by a newer version of Bond compiler
#error and is incompatible with your version Bond library.
#endif

#if BOND_MIN_CODEGEN_VERSION > 0x#{majorVersion}#{minorVersion}
#error This file was generated by an older version of Bond compiler
#error and is incompatible with your version Bond library.
#endif

#include <bond/core/config.h>
#include <bond/core/containers.h>
#{newlineSep 0 optionalHeader bondHeaders}
#{includeEnum}
#{newlineSepEnd 0 includeImport imports}
#{CPP.openNamespace cpp}
    #{doubleLineSep 1 typeDeclaration declarations}
#{CPP.closeNamespace cpp}
#{optional usesAllocatorSpecialization allocator}
|])
  where
    cppType = getTypeName cpp
    idlNamespace = getIdlQualifiedName $ getIdlNamespace cpp

    cppDefaultValue = CPP.defaultValue cpp

    includeImport (Import path) = [lt|#include "#{dropExtension path}_types.h"|]

    optionalHeader (False, _) = mempty
    optionalHeader (True, header) = includeHeader header

    includeHeader header = [lt|#include #{header}|]

    includeEnum = if enumHeader then [lt|#include "#{file}_enum.h"|] else mempty

    -- True if declarations have any type satisfying f
    have f = getAny $ foldMap g declarations
      where
        g s@Struct{..} = foldMapStructFields (foldMapType f . fieldType) s
                      <> optional (foldMapType f) structBase
        g Alias{..} = foldMapType f aliasType
        g _ = Any False

    anyBonded (BT_Bonded _) = Any True
    anyBonded _ = Any False

    anyBlob BT_Blob = Any True
    anyBlob _ = Any False

    anyNullable = Any . nullableType

    bondHeaders :: [(Bool, String)]
    bondHeaders = [
        (have anyNullable, "<bond/core/nullable.h>"),
        (have anyBonded, "<bond/core/bonded.h>"),
        (have anyBlob, "<bond/core/blob.h>")]

    usesAllocatorSpecialization alloc = [lt|
#if !defined(BOND_NO_CXX11_ALLOCATOR)
namespace std
{
    #{doubleLineSep 1 usesAllocator declarations}
}
#endif
|]
      where
        usesAllocator s@Struct {..} = [lt|template <typename _Alloc#{sepBeginBy ", typename " paramName declParams}>
    struct uses_allocator<#{typename} #{getDeclQualifiedTypeName cpp s}#{CPP.structParams s}, _Alloc>
        : is_convertible<_Alloc, #{allocParam}>
    {};|]
          where
            typename = if null declParams then mempty else [lt|typename|]
            allocParam = if last alloc == '>' then alloc ++ " " else alloc
        usesAllocator _ = mempty

    -- forward declaration
    typeDeclaration f@Forward {..} = [lt|#{CPP.template f}struct #{declName};|]

    -- struct definition
    typeDeclaration s@Struct {..} = [lt|
    #{template}struct #{declName}#{optional base structBase}
    {
        #{newlineSepEnd 2 field structFields}#{defaultCtor}

        #{copyCtor}
        #{moveCtor}
        #{optional allocatorCtor allocator}
        #{assignmentOp}

        bool operator==(const #{declName}&#{otherParam}) const
        {
            return true#{optional baseEqual structBase}#{newlineBeginSep 4 fieldEqual structFields};
        }

        bool operator!=(const #{declName}& other) const
        {
            return !(*this == other);
        }

        void swap(#{declName}&#{otherParam})
        {
            using std::swap;#{optional swapBase structBase}#{newlineBeginSep 3 swapField structFields}
        }

        struct Schema;

    protected:
        #{initMetadata}
    };

    #{template}inline void swap(#{structName}& left, #{structName}& right)
    {
        left.swap(right);
    }|]
      where
        template = CPP.template s
        structName = CPP.structName s

        otherParam = if hasOnlyMetaFields then mempty else [lt| other|]
        hasOnlyMetaFields = not (any (not . getAny . metaField) structFields) && isNothing structBase
        hasMetaFields = getAny $ foldMapStructFields metaField s

        base x = [lt|
      : #{cppType x}|]

        field Field {..} = [lt|#{cppType fieldType} #{fieldName};|]

        notMeta Field {fieldType = BT_MetaName, ..} _ = [lt|/* skip bond_meta::name field '#{fieldName}' */|]
        notMeta Field {fieldType = BT_MetaFullName, ..} _ = [lt|/* skip bond_meta::full_name field '#{fieldName}' */|]
        notMeta _ f = f

        fieldEqual f@Field {..} = notMeta f [lt|&& (#{fieldName} ==#{otherParam}.#{fieldName})|]

        baseEqual b = [lt|
                && (static_cast<const #{cppType b}&>(*this) == static_cast<const #{cppType b}&>(#{otherParam}))|]

        swapField f@Field {..} = notMeta f [lt|swap(#{fieldName},#{otherParam}.#{fieldName});|]

        swapBase b = [lt|
            #{cppType b}::swap(#{otherParam});|]

        -- value to pass to field initializer in ctor initialize list
        -- or Nothing if field doesn't need explicit initialization
        initValue (BT_Maybe _) _ = Nothing
        initValue (BT_TypeParam _) _ = Just mempty
        initValue (BT_UserDefined a@Alias {} args) d =
            case findAliasMapping cpp a of
                Nothing -> initValue (resolveAlias a args) d
                Just _ -> Just mempty
        initValue t (Just d) = Just $ cppDefaultValue t d
        initValue t _
            | scalarType t = Just mempty
            | otherwise = Nothing

        -- constructor initializer list from 'base' and 'fields' initializers
        initializeList base' fields = between colon mempty $ commaLineSep 3 id [base', fields]
          where
            colon = [lt|
          : |]

        -- constructor body
        ctorBody = if hasMetaFields then [lt|
        {
            InitMetadata("#{declName}", "#{idlNamespace}.#{declName}");
        }|]
            else [lt|
        {
        }|]

        -- default constructor
        defaultCtor = [lt|
        #{declName}()#{initList}#{ctorBody}|]
          where
            initList = initializeList mempty
                $ commaLineSep 3 fieldInit structFields
            fieldInit Field {..} = optional (\x -> [lt|#{fieldName}(#{x})|])
                $ initValue fieldType fieldDefault

        allocatorCtor alloc = [lt|
        explicit
        #{declName}(const #{alloc}&#{allocParam})#{initList}#{ctorBody}
        |]
          where
            allocParam = if needAlloc then [lt| allocator|] else mempty
              where
                needAlloc = isJust structBase || isJust (find needsAlloc structFields)
                needsAlloc Field {..} = isJust $ allocInitValue (const $ const Nothing) fieldType fieldDefault
            initList = initializeList
                (optional baseInit structBase)
                (commaLineSep 3 fieldInit structFields)
            baseInit b = [lt|#{cppType b}(allocator)|]
            fieldInit Field {..} = optional (\x -> [lt|#{fieldName}(#{x})|])
                $ allocInitValue initValue fieldType fieldDefault
            allocInitValue _ t (Just d)
                | stringType t = Just [lt|#{cppDefaultValue t d}, allocator|]
            allocInitValue _ t _
                | listType t || metaType t || stringType t || structType t = Just "allocator"
                | associativeType t = Just [lt|std::less<#{keyType t}>(), allocator|]
            allocInitValue i (BT_Nullable t) _
                | scalarType t = Nothing
                | nullableType t = allocInitValue i t Nothing
                | otherwise = Just "allocator"
            allocInitValue i (BT_Maybe t) _
                | scalarType t = Nothing
                | otherwise = allocInitValue i t Nothing
            allocInitValue i t@(BT_UserDefined a@Alias {..} args) d = if allocParameterized t
                then allocInitValue i (resolveAlias a args) d
                else Just mempty
            allocInitValue i f d = i f d
            keyType (BT_Set key) = cppType key
            keyType (BT_Map key _) = cppType key
            keyType _ = error "allocatorCtor/keyType: impossible happened."
            allocParameterized = L.isInfixOf (L.pack alloc) . toLazyText . cppType

        -- copy constructor
        copyCtor = if hasMetaFields then define else implicitlyDeclared
          where
            -- default OK when there are no meta fields
            implicitlyDeclared = CPP.ifndef CPP.defaultedFunctions [lt|
        // Compiler generated copy ctor OK
        #{declName}(const #{declName}& other) = default;|]

            -- define ctor to initialize meta fields
            define = [lt|#{declName}(const #{declName}& other)#{initList}#{ctorBody}|]
              where
                initList = initializeList
                    (optional baseCopy structBase)
                    (commaLineSep 3 fieldCopy structFields)
                baseCopy b = [lt|#{cppType b}(other)|]
                fieldCopy Field {..} = [lt|#{fieldName}(other.#{fieldName}#{getAllocator fieldType})|]
                getAllocator BT_MetaName = [lt|.get_allocator()|]
                getAllocator BT_MetaFullName =  [lt|.get_allocator()|]
                getAllocator _ = mempty

        -- move constructor
        moveCtor = CPP.ifndef CPP.rvalueReferences [lt|
        #{declName}(#{declName}&&#{param})#{initList}#{ctorBody}|]
          where
            initList = initializeList
                (optional baseMove structBase)
                (commaLineSep 3 fieldMove structFields)
            baseMove b = [lt|#{cppType b}(std::move(other))|]
            fieldMove Field {..} = [lt|#{fieldName}(std::move(other.#{fieldName}))|]
            param = if initList == mempty then mempty else [lt| other|]

        -- operator=
        assignmentOp = if hasMetaFields then define else implicitlyDeclared
          where
            -- default OK when there are no meta fields
            implicitlyDeclared = CPP.ifndef CPP.defaultedFunctions [lt|
        // Compiler generated operator= OK
        #{declName}& operator=(const #{declName}& other) = default;|]

            -- define operator= using swap
            define = [lt|#{declName}& operator=(const #{declName}& other)
        {
            #{declName}(other).swap(*this);
            return *this;
        }|]

        initMetadata = [lt|void InitMetadata(const char*#{nameParam}, const char*#{qualifiedNameParam})
        {#{newlineBeginSep 3 id [baseInit, nameInit, qualifiedInit]}
        }|]
          where
            nameParam = if baseInit == mempty && nameInit == mempty then mempty else [lt| name|]
            qualifiedNameParam = if baseInit == mempty && qualifiedInit == mempty then mempty else [lt| qualified_name|]
            baseInit = optional (\b -> [lt|#{cppType b}::InitMetadata(name, qualified_name);|]) structBase
            nameInit = newlineSep 3 init structFields
              where
                init Field {fieldType = BT_MetaName, ..} = [lt|this->#{fieldName} = name;|]
                init _ = mempty
            qualifiedInit = newlineSep 3 init structFields
              where
                init Field {fieldType = BT_MetaFullName, ..} = [lt|this->#{fieldName} = qualified_name;|]
                init _ = mempty

    -- enum definition and helpers
    typeDeclaration e@Enum {..} = [lt|
    namespace _bond_enumerators
    {
    namespace #{declName}
    {
        #{enumDefinition}
        extern const std::map<enum #{declName}, std::string> _value_to_name_#{declName};
        extern const std::map<std::string, enum #{declName}> _name_to_value_#{declName};

        inline
        const char* GetTypeName(enum #{declName})
        {
            return "#{declName}";
        }

        inline
        const char* GetTypeName(enum #{declName}, const bond::qualified_name_tag&)
        {
            return "#{idlNamespace}.#{declName}";
        }

        inline
        const std::map<enum #{declName}, std::string>& GetValueToNameMap(enum #{declName})
        {
            return _value_to_name_#{declName};
        }

        inline
        const std::map<std::string, enum #{declName}>& GetNameToValueMap(enum #{declName})
        {
            return _name_to_value_#{declName};
        }

        const std::string& ToString(enum #{declName} value);

        void FromString(const std::string& name, enum #{declName}& value);

        inline
        bool ToEnum(enum #{declName}& value, const std::string& name)
        {
            std::map<std::string, enum #{declName}>::const_iterator it =
                _name_to_value_#{declName}.find(name);

            if (_name_to_value_#{declName}.end() == it)
                return false;

            value = it->second;

            return true;
        }
    } // namespace #{declName}
    } // namespace _bond_enumerators

    #{enumUsing}|]
      where
        enumDefinition = if enumHeader then mempty else [lt|#{CPP.enumDefinition e}
        |]
        enumUsing = if enumHeader then mempty else [lt|using namespace _bond_enumerators::#{declName};
    |]

    typeDeclaration _ = mempty
