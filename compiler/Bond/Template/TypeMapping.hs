-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Bond.Template.TypeMapping
    ( findAliasMapping
    , cppTypeMapping
    , cppCustomAllocTypeMapping
    , csTypeMapping
    , csInterfaceTypeMapping
    , csAnnotatedTypeMapping
    , getNamespace
    , getIdlNamespace
    , getDeclNamespace
    , getQualifiedName
    , getIdlQualifiedName
    , getGlobalQualifiedName
    , getDeclQualifiedTypeName
    , getTypeName
    , getInstanceTypeName
    , MappingContext(..)
    , TypeMapping
    ) where

import Data.List
import Data.Monoid
import Control.Applicative
import Control.Monad.Reader
import Prelude
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder
import Text.Shakespeare.Text
import Bond.Schema.Types
import Bond.Schema.Util
import Bond.Util
import Bond.Template.Util
import Bond.Template.CustomMapping

data MappingContext = MappingContext
    { typeMapping :: TypeMapping
    , aliasMapping :: [AliasMapping]
    , namespaceMapping :: [NamespaceMapping]
    , namespaces :: [Namespace]
    }

data TypeMapping = TypeMapping
    { language :: Language
    , global :: Builder
    , separator :: Builder
    , mapType :: Type -> TypeNameBuilder
    , fixSyntax :: Builder -> Builder
    , instanceMapping :: TypeMapping
    , elementMapping :: TypeMapping
    }

type TypeNameBuilder = Reader MappingContext Builder

getNamespace :: MappingContext -> QualifiedName
getNamespace c@MappingContext {..} = resolveNamespace c namespaces

getIdlNamespace :: MappingContext -> QualifiedName
getIdlNamespace c@MappingContext {..} = findNamespace c namespaces

getDeclNamespace :: MappingContext -> Declaration -> QualifiedName
getDeclNamespace c = resolveNamespace c . declNamespaces

getQualifiedName :: TypeMapping -> QualifiedName -> Builder
getQualifiedName TypeMapping {..} = sep separator

getIdlQualifiedName :: QualifiedName -> Builder
getIdlQualifiedName = sep "."

getGlobalQualifiedName :: TypeMapping -> QualifiedName -> Builder
getGlobalQualifiedName m@TypeMapping {..} = (global <>) . getQualifiedName m

getDeclQualifiedTypeName :: MappingContext -> Declaration -> Builder
getDeclQualifiedTypeName c = getGlobalQualifiedName (typeMapping c) . declQualifiedName c

getTypeName :: MappingContext -> Type -> Builder
getTypeName c t = fix' $ runReader (typeName t) c
  where
    fix' = fixSyntax $ typeMapping c

getInstanceTypeName :: MappingContext -> Type -> Builder
getInstanceTypeName c t = runReader (instanceTypeName t) c

-- type mappings for different languages/variants
cppTypeMapping :: TypeMapping
cppTypeMapping = TypeMapping
    Cpp
    "::"
    "::"
    cppType
    cppSyntaxFix
    cppTypeMapping
    cppTypeMapping

cppCustomAllocTypeMapping :: ToText a => a -> TypeMapping
cppCustomAllocTypeMapping alloc = TypeMapping
    Cpp
    "::"
    "::"
    (cppTypeCustomAlloc $ toText alloc)
    cppSyntaxFix
    (cppCustomAllocTypeMapping alloc)
    (cppCustomAllocTypeMapping alloc)

csTypeMapping :: TypeMapping
csTypeMapping = TypeMapping
    Cs
    "global::"
    "."
    csType
    id
    csTypeMapping
    csTypeMapping

csInterfaceTypeMapping :: TypeMapping
csInterfaceTypeMapping = TypeMapping
    Cs
    "global::"
    "."
    csIfaceType
    id
    csInterfaceInstanceTypeMapping
    csInterfaceTypeMapping

csInterfaceInstanceTypeMapping :: TypeMapping
csInterfaceInstanceTypeMapping = csInterfaceTypeMapping {mapType = csType}

csAnnotatedTypeMapping :: TypeMapping
csAnnotatedTypeMapping = TypeMapping
    Cs
    "global::"
    "."
    csTypeAnnotation
    id
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

resolveNamespace :: MappingContext -> [Namespace] -> QualifiedName
resolveNamespace c@MappingContext {..} ns = maybe namespace toNamespace $ find ((namespace ==) . fromNamespace) namespaceMapping
  where
    namespace = findNamespace c ns

-- last namespace that is language-neutral or matches the language of the context's type mapping
findNamespace :: MappingContext -> [Namespace] -> QualifiedName
findNamespace MappingContext {..} ns =
    nsName . last . filter (maybe True (language typeMapping ==) . nsLanguage) $ ns

declQualifiedName :: MappingContext -> Declaration -> QualifiedName
declQualifiedName c decl = getDeclNamespace c decl ++ [declName decl]

declQualifiedTypeName :: Declaration -> TypeNameBuilder
declQualifiedTypeName decl = do
    ctx <- ask
    return $ getDeclQualifiedTypeName ctx decl

declTypeName :: Declaration -> TypeNameBuilder
declTypeName decl = do
    ctx <- ask
    if namespaces ctx == declNamespaces decl
            then pureText $ declName decl
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
cppTypeCustomAlloc alloc (BT_Nullable element) | structType element = "bond::nullable<" <>> elementTypeName element <<> ", " <> alloc <> ">"
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
csType (BT_Nullable element) = typeName element <<> if scalarType element then "?" else mempty
csType (BT_List element) = "LinkedList<" <>> elementTypeName element <<> ">"
csType (BT_Vector element) = "List<" <>> elementTypeName element <<> ">"
csType (BT_Set element) = "HashSet<" <>> elementTypeName element <<> ">"
csType (BT_Map key value) = "Dictionary<" <>> elementTypeName key <<>> ", " <>> elementTypeName value <<> ">"
csType (BT_Bonded type_) = "global::Bond.IBonded<" <>> typeName type_ <<> ">"
csType (BT_TypeParam param) = pureText $ paramName param
csType (BT_UserDefined a@Alias {} args) = aliasTypeName a args
csType (BT_UserDefined decl args) = declTypeName decl <<>> (angles <$> localWith (const csTypeMapping) (commaSepTypeNames args))

-- C# type mapping with collection interfaces
csIfaceType :: Type -> TypeNameBuilder
csIfaceType (BT_List element) = "ICollection<" <>> elementTypeName element <<> ">"
csIfaceType (BT_Vector element) = "IList<" <>> elementTypeName element <<> ">"
csIfaceType (BT_Set element) = "ISet<" <>> elementTypeName element <<> ">"
csIfaceType (BT_Map key value) = "IDictionary<" <>> elementTypeName key <<>> ", " <>> elementTypeName value <<> ">"
csIfaceType t = csType t

-- C# type annotation mapping
csTypeAnnotation :: Type -> TypeNameBuilder
csTypeAnnotation BT_WString = pure "global::Bond.Tag.wstring"
csTypeAnnotation (BT_Nullable element) = "global::Bond.Tag.nullable<" <>> typeName element <<> ">"
csTypeAnnotation (BT_Bonded type_) = "global::Bond.Tag.bonded<" <>> typeName type_ <<> ">"
csTypeAnnotation (BT_TypeParam (TypeParam _ Nothing)) = pure "global::Bond.Tag.classT"
csTypeAnnotation (BT_TypeParam (TypeParam _ (Just Value))) = pure "global::Bond.Tag.structT"
csTypeAnnotation (BT_UserDefined Alias {aliasType = BT_Blob} _) = pure "global::Bond.Tag.blob"
csTypeAnnotation t@(BT_UserDefined a@Alias {..} args)
   | containerType t = csType t 
   | otherwise = typeName $ resolveAlias a args
csTypeAnnotation (BT_UserDefined decl args) = declTypeName decl <<>> (angles <$> commaSepTypeNames args)
csTypeAnnotation t = csType t

