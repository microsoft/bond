-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

{-|
Copyright   : (c) Microsoft
License     : MIT
Maintainer  : adamsap@microsoft.com
Stability   : alpha
Portability : portable

The module defines types and functions abstracting mapping between Bond types
and types in the code generation target language. Codegen templates can be used
with different type name mappings to customize generated code.
-}

module Language.Bond.Codegen.TypeMapping
    ( -- * Mapping context
      MappingContext(..)
    , TypeMapping
      -- * Type mappings
    , idlTypeMapping
    , cppTypeMapping
    , cppCustomAllocTypeMapping
    , csTypeMapping
    , csCollectionInterfacesTypeMapping
      -- * Name builders
    , getTypeName
    , getInstanceTypeName
    , getAnnotatedTypeName
    , getDeclTypeName
    , getQualifiedName
    , getGlobalQualifiedName
    , getIdlQualifiedName
      -- * Helper functions
    , getNamespace
    , getIdlNamespace
    , getDeclName
    , getDeclNamespace
    , customAliasMapping
    ) where

import Data.List
import Data.Monoid
import Control.Applicative
import Control.Monad.Reader
import Prelude
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder
import Text.Shakespeare.Text
import Language.Bond.Syntax.Types
import Language.Bond.Syntax.Util
import Language.Bond.Util
import Language.Bond.Codegen.CustomMapping

-- | The context that determines how a Bond 'Type' maps to a type name in
-- generated code. A context applies to code generation of one or more files in
-- the target language based on the information from a parsed Bond schema
-- definition file. The namespace from the schema definition is stored in the
-- context and all non-qualified names mapped in the context are considered to
-- be defined in that namespace.
data MappingContext = MappingContext
    { typeMapping :: TypeMapping
    , aliasMapping :: [AliasMapping]
    , namespaceMapping :: [NamespaceMapping]
    , namespaces :: [Namespace]
    }

-- | Opaque type representing type mapping.
data TypeMapping = TypeMapping
    { language :: Maybe Language
    , global :: Builder
    , separator :: Builder
    , mapType :: Type -> TypeNameBuilder
    , mapDeclName :: Declaration -> String
    , fixSyntax :: Builder -> Builder
    , instanceMapping :: TypeMapping
    , elementMapping :: TypeMapping
    , annotatedMapping :: TypeMapping
    }

type TypeNameBuilder = Reader MappingContext Builder

-- | Returns the namespace for the 'MappingContext'. The namespace may be
-- different than specified in schema definition file if namespace mapping is
-- in effect.
getNamespace :: MappingContext -> QualifiedName
getNamespace c@MappingContext {..} = resolveNamespace c namespaces

-- | Returns the namespace for the 'MappingContext' as specified in the schema
-- definition file (i.e. ignoring namespace mapping).
getIdlNamespace :: MappingContext -> QualifiedName
getIdlNamespace c@MappingContext {..} = findNamespace c namespaces

-- | Returns the namespace for a 'Declaration' in the specified 'MappingContext'.
getDeclNamespace :: MappingContext -> Declaration -> QualifiedName
getDeclNamespace c = resolveNamespace c . declNamespaces

-- | Builds a qualified name in the specified 'MappingContext'.
getQualifiedName :: MappingContext -> QualifiedName -> Builder
getQualifiedName MappingContext { typeMapping = m } = sepBy (separator m) toText

-- | Builds qualified name in schema definition context (i.e. independent of
-- the code generation target language).
getIdlQualifiedName :: QualifiedName -> Builder
getIdlQualifiedName = sepBy "." toText

-- | Builds a global qualified name in the specified 'MappingContext'.
getGlobalQualifiedName :: MappingContext -> QualifiedName -> Builder
getGlobalQualifiedName c@MappingContext { typeMapping = m } = (global m <>) . getQualifiedName c

-- | Returns name of the declaration in given mapping context
getDeclName :: MappingContext -> Declaration -> String
getDeclName MappingContext {..} = mapDeclName typeMapping

-- | Builds the qualified name for a 'Declaration' in the specified
-- 'MappingContext'.
getDeclTypeName :: MappingContext -> Declaration -> Builder
getDeclTypeName c = getGlobalQualifiedName c . declQualifiedName c

-- | Builds the name of a 'Type' in the specified 'MappingContext'.
getTypeName :: MappingContext -> Type -> Builder
getTypeName c t = fix' $ runReader (typeName t) c
  where
    fix' = fixSyntax $ typeMapping c

-- | Builds the name to be used when instantiating a 'Type'. The instance type
-- name may be different than the type name returned by 'getTypeName' when the
-- latter is an interface.
getInstanceTypeName :: MappingContext -> Type -> Builder
getInstanceTypeName c t = runReader (instanceTypeName t) c

-- | Builds the annotated name of a 'Type'. The type annotations are used to
-- express type information about a Bond type that doesn't directly map to
-- the target language type system (e.g. distinction between a nullable and
-- non-nullable string in C# type system).
getAnnotatedTypeName :: MappingContext -> Type -> Builder
getAnnotatedTypeName c t = runReader (annotatedTypeName t) c

-- | Returns 'True' if the alias has a custom mapping in the given
-- 'MappingContext'.
customAliasMapping :: MappingContext -> Declaration -> Bool
customAliasMapping = (maybe False (const True) .) . findAliasMapping

-- | The IDL type name mapping.
idlTypeMapping :: TypeMapping
idlTypeMapping = TypeMapping
    Nothing
    ""
    "."
    idlType
    declName
    id
    idlTypeMapping
    idlTypeMapping
    idlTypeMapping

-- | The default C++ type name mapping.
cppTypeMapping :: TypeMapping
cppTypeMapping = TypeMapping
    (Just Cpp)
    "::"
    "::"
    cppType
    declName
    cppSyntaxFix
    cppTypeMapping
    cppTypeMapping
    cppTypeMapping

-- | C++ type name mapping using a custom allocator.
cppCustomAllocTypeMapping :: ToText a => a -> TypeMapping
cppCustomAllocTypeMapping alloc = TypeMapping
    (Just Cpp)
    "::"
    "::"
    (cppTypeCustomAlloc $ toText alloc)
    declName
    cppSyntaxFix
    (cppCustomAllocTypeMapping alloc)
    (cppCustomAllocTypeMapping alloc)
    (cppCustomAllocTypeMapping alloc)

-- | The default C# type name mapping.
csTypeMapping :: TypeMapping
csTypeMapping = TypeMapping
    (Just Cs)
    "global::"
    "."
    csType
    declName
    id
    csTypeMapping
    csTypeMapping
    csAnnotatedTypeMapping

-- | C# type name mapping using interfaces rather than concrete types to
-- represent collections.
csCollectionInterfacesTypeMapping :: TypeMapping
csCollectionInterfacesTypeMapping = TypeMapping
    (Just Cs)
    "global::"
    "."
    csInterfaceType
    declName
    id
    csCollectionInstancesTypeMapping
    csCollectionInterfacesTypeMapping
    csAnnotatedTypeMapping

csCollectionInstancesTypeMapping :: TypeMapping
csCollectionInstancesTypeMapping = csCollectionInterfacesTypeMapping {mapType = csType}

csAnnotatedTypeMapping :: TypeMapping
csAnnotatedTypeMapping = TypeMapping
    (Just Cs)
    "global::"
    "."
    (csTypeAnnotation csType)
    declName
    id
    csAnnotatedTypeMapping
    csAnnotatedTypeMapping
    csAnnotatedTypeMapping

infixr 6 <<>>

(<<>>) :: (Monoid r, Monad m) => m r -> m r -> m r
(<<>>) = liftM2 (<>)

infixr 6 <>>

(<>>) :: (Monoid r, Monad m) => r -> m r -> m r
(<>>) x = liftM (x <>)

infixr 6 <<>

(<<>) :: (Monoid r, Monad m) => m r -> r -> m r
(<<>) x y = liftM (<> y) x

pureText :: ToText a => a -> TypeNameBuilder
pureText = pure . toText

commaSepTypeNames :: [Type] -> TypeNameBuilder
commaSepTypeNames [] = return mempty
commaSepTypeNames [x] = typeName x
commaSepTypeNames (x:xs) = typeName x <<>> ", " <>> commaSepTypeNames xs

typeName :: Type -> TypeNameBuilder
typeName t = do
    m <- asks $ mapType . typeMapping
    m t

localWith :: (TypeMapping -> TypeMapping) -> TypeNameBuilder -> TypeNameBuilder
localWith f = local $ \c -> c { typeMapping = f $ typeMapping c }

elementTypeName :: Type -> TypeNameBuilder
elementTypeName = localWith elementMapping . typeName

instanceTypeName :: Type -> TypeNameBuilder
instanceTypeName = localWith instanceMapping . typeName

annotatedTypeName :: Type -> TypeNameBuilder
annotatedTypeName = localWith annotatedMapping . typeName

resolveNamespace :: MappingContext -> [Namespace] -> QualifiedName
resolveNamespace c@MappingContext {..} ns = maybe namespace toNamespace $ find ((namespace ==) . fromNamespace) namespaceMapping
  where
    namespace = findNamespace c ns

-- last namespace that is language-neutral or matches the language of the context's type mapping
findNamespace :: MappingContext -> [Namespace] -> QualifiedName
findNamespace MappingContext {..} ns =
    nsName . last . filter (matching (language typeMapping) . nsLanguage) $ ns
  where  
    matching (Just l1) (Just l2) = l1 == l2
    matching _ _ = True

declQualifiedName :: MappingContext -> Declaration -> QualifiedName
declQualifiedName c decl = getDeclNamespace c decl ++ [declName decl]

declQualifiedTypeName :: Declaration -> TypeNameBuilder
declQualifiedTypeName decl = do
    ctx <- ask
    return $ getDeclTypeName ctx decl

declTypeName :: Declaration -> TypeNameBuilder
declTypeName decl = do
    ctx <- ask
    if namespaces ctx == declNamespaces decl
            then pureText $ getDeclName ctx decl
            else declQualifiedTypeName decl

findAliasMapping :: MappingContext -> Declaration -> Maybe AliasMapping
findAliasMapping ctx a = find isSameAlias $ aliasMapping ctx
  where
    aliasDeclName = declQualifiedName ctx a
    isSameNs = namespaces ctx == declNamespaces a
    isSameAlias m = aliasDeclName == aliasName m || isSameNs && [declName a] == aliasName m

aliasTypeName :: Declaration -> [Type] -> TypeNameBuilder
aliasTypeName a args = do
    ctx <- ask
    case findAliasMapping ctx a of
        Just AliasMapping {..} -> foldr ((<<>>) . fragment) (pure mempty) aliasTemplate
        Nothing -> typeName $ resolveAlias a args
  where
    fragment (Fragment s) = pureText s
    fragment (Placeholder i) = typeName $ args !! i

-- IDL type mapping
idlType :: Type -> TypeNameBuilder
idlType BT_Int8 = pure "int8"
idlType BT_Int16 = pure "int16"
idlType BT_Int32 = pure "int32"
idlType BT_Int64 = pure "int64"
idlType BT_UInt8 = pure "uint8"
idlType BT_UInt16 = pure "uint16"
idlType BT_UInt32 = pure "uint32"
idlType BT_UInt64 = pure "uint64"
idlType BT_Float = pure "float"
idlType BT_Double = pure "double"
idlType BT_Bool = pure "bool"
idlType BT_String = pure "string"
idlType BT_WString = pure "wstring"
idlType BT_MetaName = pure "bond_meta::name"
idlType BT_MetaFullName = pure "bond_meta::full_name"
idlType BT_Blob = pure "blob"
idlType (BT_IntTypeArg x) = pureText x
idlType (BT_Maybe type_) = elementTypeName type_
idlType (BT_List element) = "list<" <>> elementTypeName element <<> ">"
idlType (BT_Nullable element) = "nullable<" <>> elementTypeName element <<> ">"
idlType (BT_Vector element) = "vector<" <>> elementTypeName element <<> ">"
idlType (BT_Set element) = "set<" <>> elementTypeName element <<> ">"
idlType (BT_Map key value) = "map<" <>> elementTypeName key <<>> ", " <>> elementTypeName value <<> ">"
idlType (BT_Bonded type_) = "bonded<" <>> elementTypeName type_ <<> ">"
idlType (BT_TypeParam param) = pureText $ paramName param
idlType (BT_UserDefined a@Alias {..} args) = aliasTypeName a args
idlType (BT_UserDefined decl args) = declQualifiedTypeName decl <<>> (angles <$> commaSepTypeNames args)

-- C++ type mapping
cppType :: Type -> TypeNameBuilder
cppType BT_Int8 = pure "int8_t"
cppType BT_Int16 = pure "int16_t"
cppType BT_Int32 = pure "int32_t"
cppType BT_Int64 = pure "int64_t"
cppType BT_UInt8 = pure "uint8_t"
cppType BT_UInt16 = pure "uint16_t"
cppType BT_UInt32 = pure "uint32_t"
cppType BT_UInt64 = pure "uint64_t"
cppType BT_Float = pure "float"
cppType BT_Double = pure "double"
cppType BT_Bool = pure "bool"
cppType BT_String = pure "std::string"
cppType BT_WString = pure "std::wstring"
cppType BT_MetaName = pure "std::string"
cppType BT_MetaFullName = pure "std::string"
cppType BT_Blob = pure "bond::blob"
cppType (BT_IntTypeArg x) = pureText x
cppType (BT_Maybe type_) = "bond::maybe<" <>> elementTypeName type_ <<> ">"
cppType (BT_List element) = "std::list<" <>> elementTypeName element <<> ">"
cppType (BT_Nullable element) = "bond::nullable<" <>> elementTypeName element <<> ">"
cppType (BT_Vector element) = "std::vector<" <>> elementTypeName element <<> ">"
cppType (BT_Set element) = "std::set<" <>> elementTypeName element <<> ">"
cppType (BT_Map key value) = "std::map<" <>> elementTypeName key <<>> ", " <>> elementTypeName value <<> ">"
cppType (BT_Bonded type_) = "bond::bonded<" <>> elementTypeName type_ <<> ">"
cppType (BT_TypeParam param) = pureText $ paramName param
cppType (BT_UserDefined a@Alias {..} args) = aliasTypeName a args
cppType (BT_UserDefined decl args) = declQualifiedTypeName decl <<>> (angles <$> commaSepTypeNames args)

-- C++ type mapping with custom allocator
cppTypeCustomAlloc :: Builder -> Type -> TypeNameBuilder
cppTypeCustomAlloc alloc BT_String = pure $ "std::basic_string<char, std::char_traits<char>, typename " <> alloc <> "::rebind<char>::other>"
cppTypeCustomAlloc alloc BT_WString = pure $ "std::basic_string<wchar_t, std::char_traits<wchar_t>, typename " <> alloc <>  "::rebind<wchar_t>::other>"
cppTypeCustomAlloc alloc BT_MetaName = cppTypeCustomAlloc alloc BT_String
cppTypeCustomAlloc alloc BT_MetaFullName = cppTypeCustomAlloc alloc BT_String
cppTypeCustomAlloc alloc (BT_List element) = "std::list<" <>> elementTypeName element <<>> ", " <>> allocator alloc element <<> ">"
cppTypeCustomAlloc alloc (BT_Nullable element) | isStruct element = "bond::nullable<" <>> elementTypeName element <<> ", " <> alloc <> ">"
cppTypeCustomAlloc _lloc (BT_Nullable element) = "bond::nullable<" <>> elementTypeName element <<> ">"
cppTypeCustomAlloc alloc (BT_Vector element) = "std::vector<" <>> elementTypeName element <<>> ", " <>> allocator alloc element <<> ">"
cppTypeCustomAlloc alloc (BT_Set element) = "std::set<" <>> elementTypeName element <<>> comparer element <<>> allocator alloc element <<> ">"
cppTypeCustomAlloc alloc (BT_Map key value) = "std::map<" <>> elementTypeName key <<>> ", " <>> elementTypeName value <<>> comparer key <<>> pairAllocator alloc key value <<> ">"
cppTypeCustomAlloc _ t = cppType t

comparer :: Type -> TypeNameBuilder
comparer t = ", std::less<" <>> elementTypeName t <<> ">, "

allocator :: Builder -> Type -> TypeNameBuilder
allocator alloc element =
    "typename " <>> alloc <>> "::rebind<" <>> elementTypeName element <<> ">::other"

pairAllocator :: Builder -> Type -> Type -> TypeNameBuilder
pairAllocator alloc key value =
    "typename " <>> alloc <>> "::rebind<" <>> "std::pair<const " <>> elementTypeName key <<>> ", " <>> elementTypeName value <<> "> >::other"

cppSyntaxFix :: Builder -> Builder
cppSyntaxFix = fromLazyText . snd . L.foldr fixInvalid (' ', mempty) . toLazyText
  where
    fixInvalid c r
        -- C++98 requires space between consecutive angle brackets
        | c == '>' && fst r == '>' = (c, L.cons c (L.cons ' ' $ snd r))
        -- <: is digraph for [
        | c == '<' && fst r == ':' = (c, L.cons c (L.cons ' ' $ snd r))
        | otherwise = (c, L.cons c (snd r))


-- C# type mapping
csType :: Type -> TypeNameBuilder
csType BT_Int8 = pure "sbyte"
csType BT_Int16 = pure "short"
csType BT_Int32 = pure "int"
csType BT_Int64 = pure "long"
csType BT_UInt8 = pure "byte"
csType BT_UInt16 = pure "ushort"
csType BT_UInt32 = pure "uint"
csType BT_UInt64 = pure "ulong"
csType BT_Float = pure "float"
csType BT_Double = pure "double"
csType BT_Bool = pure "bool"
csType BT_String = pure "string"
csType BT_WString = pure "string"
csType BT_MetaName = pure "string"
csType BT_MetaFullName = pure "string"
csType BT_Blob = pure "System.ArraySegment<byte>"
csType (BT_IntTypeArg x) = pureText x
csType (BT_Maybe type_) = csType (BT_Nullable type_)
csType (BT_Nullable element) = typeName element <<> if isScalar element then "?" else mempty
csType (BT_List element) = "LinkedList<" <>> elementTypeName element <<> ">"
csType (BT_Vector element) = "List<" <>> elementTypeName element <<> ">"
csType (BT_Set element) = "HashSet<" <>> elementTypeName element <<> ">"
csType (BT_Map key value) = "Dictionary<" <>> elementTypeName key <<>> ", " <>> elementTypeName value <<> ">"
csType (BT_Bonded type_) = "global::Bond.IBonded<" <>> typeName type_ <<> ">"
csType (BT_TypeParam param) = pureText $ paramName param
csType (BT_UserDefined a@Alias {} args) = aliasTypeName a args
csType (BT_UserDefined decl args) = declTypeName decl <<>> (angles <$> localWith (const csTypeMapping) (commaSepTypeNames args))

-- C# type mapping with collection interfaces
csInterfaceType :: Type -> TypeNameBuilder
csInterfaceType (BT_List element) = "ICollection<" <>> elementTypeName element <<> ">"
csInterfaceType (BT_Vector element) = "IList<" <>> elementTypeName element <<> ">"
csInterfaceType (BT_Set element) = "ISet<" <>> elementTypeName element <<> ">"
csInterfaceType (BT_Map key value) = "IDictionary<" <>> elementTypeName key <<>> ", " <>> elementTypeName value <<> ">"
csInterfaceType t = csType t

-- C# type annotation mapping
csTypeAnnotation :: (Type -> TypeNameBuilder) -> Type -> TypeNameBuilder
csTypeAnnotation _ BT_WString = pure "global::Bond.Tag.wstring"
csTypeAnnotation _ (BT_Nullable element) = "global::Bond.Tag.nullable<" <>> typeName element <<> ">"
csTypeAnnotation _ (BT_Bonded type_) = "global::Bond.Tag.bonded<" <>> typeName type_ <<> ">"
csTypeAnnotation _ (BT_Maybe a@(BT_UserDefined Alias{} _)) = typeName a
csTypeAnnotation _ (BT_TypeParam (TypeParam _ Nothing)) = pure "global::Bond.Tag.classT"
csTypeAnnotation _ (BT_TypeParam (TypeParam _ (Just Value))) = pure "global::Bond.Tag.structT"
csTypeAnnotation _ (BT_UserDefined Alias {aliasType = BT_Blob} _) = pure "global::Bond.Tag.blob"
csTypeAnnotation m t@(BT_UserDefined a@Alias {..} args)
   | isContainer t = m t
   | otherwise = typeName $ resolveAlias a args
csTypeAnnotation _ (BT_UserDefined decl args) = declTypeName decl <<>> (angles <$> commaSepTypeNames args)
csTypeAnnotation m t = m t

